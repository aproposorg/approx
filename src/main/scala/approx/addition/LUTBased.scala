package approx.addition

import chisel3._

/** LUT-based approximate adder
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part
 *                    (must be less than or equal to the width)
 * 
 * Implementation of the adder from Becher [2016]
 */
class LUTBased(width: Int, val approxWidth: Int) extends Adder(width) {
  require(approxWidth <= width,
    "width of the approximate part must be less than the width")

  // Generate lower part (i.e., m bits)
  val all1   = (io.a(approxWidth-1) ^ io.b(approxWidth-1)) & (io.a(approxWidth-2) & io.b(approxWidth-2))
  val mm2Sum = (io.a(approxWidth-2, 0) + io.b(approxWidth-2, 0) + io.cin) | all1
  val fa     = Module(new FullAdder)
  fa.io.x   := io.a(approxWidth-1)
  fa.io.y   := io.b(approxWidth-1)
  fa.io.cin := (io.a(approxWidth-2) & io.b(approxWidth-2))
  val mm1Sum = fa.io.s
  val lspSum = mm1Sum ## mm2Sum
  
  // Generate upper part
  val cinUp  = io.a(approxWidth-1) & io.b(approxWidth-1)
  val mspSum = io.a(width-1, width-approxWidth) +& io.b(width-1, width-approxWidth) + cinUp

  // Combine results and output
  io.s    := mspSum(mspSum.getWidth-2, 0) ## lspSum
  io.cout := mspSum(mspSum.getWidth-1)
}
