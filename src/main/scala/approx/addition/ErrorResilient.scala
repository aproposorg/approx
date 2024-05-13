package approx.addition

import chisel3._

/** AFA approximate full adder
 * 
 * Implementation of the AFA full adder from Dutt et al. [2016]
 */
class AFA extends FA {
  io.s := io.x ^ io.y ^ io.cin
  io.cout := io.x
}

/** Error resilient approximate adder w/o correction
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the adder from Dutt et al. [2016]
 */
class ErrorResilient(width: Int, val approxWidth: Int) extends Adder(width) {
  require(approxWidth <= width,
    "width of the approximate part must be less than or equal to the total width")

  // Generate full row of full adders
  val sums = Wire(Vec(width, Bool()))
  val cins = Wire(Vec(width+1, Bool()))
  cins(0) := io.cin
  (0 until width).foreach { i =>
    val fa = if (i < approxWidth) Module(new AFA) else Module(new FullAdder)
    fa.io.x   := io.a(i)
    fa.io.y   := io.b(i)
    fa.io.cin := cins(i)
    sums(i)   := fa.io.s
    cins(i+1) := fa.io.cout
  }

  // Combine results and output
  io.s := sums.asUInt
  io.cout := cins(width)
}

/** Error resilient approximate adder w/ correction
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the adder from Dutt et al. [2016]
 */
class ErrorResilientCorrect(width: Int, val approxWidth: Int) extends Adder(width) {
  require(approxWidth <= width,
    "width of the approximate part must be less than or equal to the total width")

  // Instantiate an error resilient adder
  val adderSums = Wire(Vec(width, Bool()))
  val adder = Module(new ErrorResilient(width, approxWidth))
  adder.io.a   := io.a
  adder.io.b   := io.b
  adder.io.cin := io.cin
  adderSums := adder.io.s.asTypeOf(adderSums)

  // Generate correction logic
  val am1s  = io.a(approxWidth-1, 0) ## false.B
  val efs   = Wire(Vec(approxWidth, Bool()))
  val cSums = Wire(Vec(approxWidth, Bool()))
  cSums := adderSums.asUInt()(approxWidth+1, 1).asTypeOf(cSums)
  val sums  = Wire(Vec(width, Bool()))
  sums(0) := adderSums(0)
  (0 until approxWidth).foreach { i =>
    efs(i) := (!io.a(i) & io.b(i) & am1s(i)) | (io.a(i) & !io.b(i) & !am1s(i))
    sums(i+1) := cSums(i) ^ efs(i)
  }
  (approxWidth+1 until width).foreach { i =>
    sums(i) := adderSums(i)
  }

  // Combine results and output
  io.s := sums.asUInt
  io.cout := adder.io.cout
}
