package approx.addition

import chisel3._
import chisel3.util.log2Up

/** Adaptive optimized lower-part constant-OR adder IO bundle
 * 
 * @param width the width of the adder
 * @param numModes the number of approximation modes
 * 
 * Ctrl input is used for selecting the approximation mode (0 means exact).
 */
class AdaptiveOFLOCAIO(width: Int, numModes: Int) extends AdderIO(width) {
  val ctrl = Input(UInt(log2Up(numModes+1).W))
}

/** Adaptive optimized lower-part constant-OR adder
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part
 *                    (must be less than or equal to the width)
 * @param numModes the number of approximation modes (defaults to 1)
 * 
 * Adaptation of the adder from Dalloo [2018]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class AdaptiveOFLOCA(val width: Int, val approxWidth: Int, val numModes: Int = 1)
  extends Module {
  require(approxWidth <= width,
    "width of the approximate part must be less than or equal to the total width")
  require(numModes >= 1, "number of approximation modes must be positive")

  val io = IO(new AdaptiveOFLOCAIO(width, numModes))

  val sums    = Wire(Vec(width,   Bool()))
  val carries = Wire(Vec(width+1, Bool()))

  // Instantiate all the full adders
  val adders = Seq.fill(width) { Module(new FullAdder) }
  (0 until width).foreach { i =>
    adders(i).io.x := io.a(i)
    adders(i).io.y := io.b(i)
    adders(i).io.cin := carries(i)
  }
  // Assign the intermediate signals default values from the full adders
  carries(0) := io.cin
  (0 until width).foreach { i =>
    sums(i)      := adders(i).io.s
    carries(i+1) := adders(i).io.cout
  }

  // Determine the distribution of bits to the different parts in each 
  // approximation mode; 0 meaning fully exact addition
  val modeDists = Seq(0) ++ (1 until numModes).map(m => approxWidth / numModes * m) ++ Seq(approxWidth)

  // Now generate the adder parts by "filling in" the required sum and carry 
  // bits below then approxWidth'th index and generating aNbN
  val aNbN = WireDefault(false.B)
  val fb   = aNbN | carries(width)
  modeDists.zipWithIndex.foreach { case (mAWidth, mInd) =>
    val mCWidth = mAWidth / 2
    when(io.ctrl === mInd.U) {
      // Generate the approximate and constant sum and carry bits
      (0 until mCWidth).foreach { i =>
        sums(i)    := true.B
        carries(i) := false.B
      }
      (mCWidth until mAWidth).foreach { i =>
        sums(i)    := (io.a(i) | io.b(i)) | fb
        carries(i) := false.B
      }
      if (mAWidth != 0) aNbN := io.a(mAWidth-1) ^ io.b(mAWidth-1)

      // Fill in the remaining sum and carries with full adders
      if (mAWidth != 0) carries(mAWidth) := io.a(mAWidth-1) & io.b(mAWidth-1)
      (mAWidth until approxWidth).foreach { i =>
        sums(i)      := adders(i).io.s
        carries(i+1) := adders(i).io.cout
      }
    }
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := carries(width)
}
