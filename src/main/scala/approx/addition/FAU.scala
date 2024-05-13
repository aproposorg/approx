package approx.addition

import chisel3._

/** FAU LUT-based approximate adder
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * @param passOverWidth the number of passed-over bits (must be less than the approximate width)
 * 
 * Implementation of the adder from Echavarria et al. [2016]
 */
class FAU(width: Int, val approxWidth: Int, val passOverWidth: Int) extends Adder(width) {
  require(approxWidth <= width,
    "width of the approximate part must be less than the width")
  require(passOverWidth < approxWidth,
    "number of passed-over bits must be less than the approximate width")

  /** Generate the carry-in to the MSP
   * 
   * @param a the first operand
   * @param b the second operand
   */
  private[FAU] def genCin(a: UInt, b: UInt) = {
    require(a.widthKnown && b.widthKnown)
    require(a.getWidth == b.getWidth && a.getWidth == passOverWidth)
    val (p, g) = (a ^ b, a & b)
    val couts  = Wire(Vec(passOverWidth+1, Bool()))
    couts(0)  := false.B
    (1 to passOverWidth).foreach { i =>
      couts(i) := g(i-1) | (p(i-1) && couts(i-1))
    }
    couts(passOverWidth)
  }

  // Generate lower part (i.e., m bits)
  val all1 = (io.a(approxWidth-1) ^ io.b(approxWidth-1)) & (io.a(approxWidth-2) & io.b(approxWidth-2))
  val mm2Sum = (io.a(approxWidth-2, 0) + io.b(approxWidth-2, 0) + io.cin) | all1
  val fa     = Module(new FullAdder)
  fa.io.x   := io.a(approxWidth-1)
  fa.io.y   := io.b(approxWidth-1)
  fa.io.cin := (io.a(approxWidth-2) & io.b(approxWidth-2))
  val mm1Sum = fa.io.s
  val lspSum = mm1Sum ## mm2Sum

  // Generate upper part
  val cinUp  = genCin(io.a(approxWidth-1, approxWidth-passOverWidth), io.b(approxWidth-1, approxWidth-passOverWidth))
  val mspSum = io.a(width-1, width-approxWidth) +& io.b(width-1, width-approxWidth) + cinUp

  // Combine results and output
  io.s    := mspSum(mspSum.getWidth-2, 0) ## lspSum
  io.cout := mspSum(mspSum.getWidth-1)
}
