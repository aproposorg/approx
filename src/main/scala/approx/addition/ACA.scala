package approx.addition

import chisel3._
import chisel3.util.isPow2

/** Accuracy-configurable approximate adder IO bundle
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages
 * 
 * Ctrl input is used for enabling error correction of least significant portions of the sum.
 */
class ACAIO(w: Int, stages: Int) extends AdderIO(w) {
  val ctrl = Input(Vec(stages-2, Bool()))
}

/** Accuracy-configurable approximate adder
 * 
 * @param width the width of the adder (must be a power of 2)
 * @param subWidth the width of the sub-adders (must be a power of 2 and at least 2 and less than the width)
 * 
 * Implementation of the adder from Kahng and Kang [2012]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class ACA(val width: Int, val subWidth: Int) extends Module {
  val stride = subWidth / 2
  val stages = (width / stride) - 1
  require(isPow2(width), "the width must be a power of 2")
  require(isPow2(subWidth),
    "the width of the sub-adders must be a power of 2")
  require(subWidth < width,
    "the width of the sub-adders must be less than the total width")
  require(subWidth >= 2,
    "the width of the sub-adders must be greater than or equal to 2")

  val io = IO(new ACAIO(width, stages))

  /** Increment by 1
   * 
   * @param a the operand
   * @return a incremented by 1
   */
  private[ACA] def increment(a: UInt) = {
    require(a.widthKnown, "width of operand should be known")
    val w = a.getWidth
    val sums  = Wire(Vec(w, Bool()))
    val couts = Wire(Vec(w, Bool()))
    (0 until w).foreach { i =>
      val ha = Module(new HalfAdder)
      if (i == 0)
        ha.io.x := 1.U
      else
        ha.io.x := couts(i-1)
      ha.io.y   := a(i)
      sums(i)   := ha.io.s
      couts(i)  := ha.io.cout
    }
    couts(w-1) ## sums.asUInt
  }

  // Partial sums
  val psums = Wire(Vec(stages, Vec(2, Vec(subWidth, Bool()))))
  (0 until stages).foreach { s =>
    (0 until subWidth).foreach { i =>
      val fa = Module(new FullAdder)
      fa.io.x := io.a(s*stride + i)
      fa.io.y := io.b(s*stride + i)
      if (s == 0 && i == 0)
        fa.io.cin := io.cin
      else if (i == 0)
        fa.io.cin := false.B
      else
        fa.io.cin := psums(s)(1)(i-1)
      psums(s)(0)(i) := fa.io.s
      psums(s)(1)(i) := fa.io.cout
    }
  }

  // Create stages-1 controllable correction stages
  val csums = Wire(Vec(stages, Vec(2, Vec(subWidth, Bool()))))
  csums(0) := psums(0)
  csums(1) := psums(1)
  (2 until stages).foreach { s =>
    val sum = psums(s)(0).asUInt
    val error = (sum(subWidth/2-1, 0) ## csums(s-1)(1)(subWidth/2-1)).andR
    when(io.ctrl(s-2) & error) { // Correct error
      csums(s)(0) := (increment(sum(subWidth-1, subWidth/2)) ## sum(subWidth/2-1, 0)).asTypeOf(csums(s)(0))
      csums(s)(1) := psums(s)(1)    
    }.otherwise { // Leave error uncorrected
      csums(s) := psums(s)
    }
  }

  // Combine results and output
  io.s := ((1 until stages).map { 
    s => csums(s)(0).asUInt(subWidth-1, subWidth/2) 
  }).reduce(_ ## _) ## csums(0)(0).asUInt
  io.cout := csums(stages-1)(1)(subWidth-1)
}
