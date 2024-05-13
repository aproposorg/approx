package approx.multiplication

import chisel3._

/** Approximate low-power high-performance multiplier with configurable partial 
 * error recovery
 * 
 * @param width the width of the multiplier
 * @param rcvrWidth the width of the error recovery part
 * 
 * Implementation of the multiplier of Liu et al. [2014]
 * 
 * Only works for unsigned numbers.
 */
class CPER(width: Int, rcvrWidth: Int) extends Multiplier(width, width) {
  require(rcvrWidth < 2*width,
    "the error recovery part must be shorter than the product width")

  /** Recursively build an approximate tree compressor with broken adder cells
   * 
   * @param cps the current partial products
   * @param cers the current error recovery vectors
   * @return a single row of compressed sum bits and a list of error recovery vectors
   */
  private[multiplication] def atc(cps: Seq[UInt], cers: Seq[UInt] = Seq.empty[UInt]): (UInt, Seq[UInt]) = {
    val bACs = cps.sliding(2, 2).map { _ match {
      case pr if pr.size == 2 =>
        val (op1, op2) = (pr.head, pr.last)
        val res = (0 until 2*width).map { i =>
          val xor = op1(i) ^ op2(i)
          val and = if (i == 0) false.B else (op1(i-1) & op2(i-1))
          (xor | and, xor & and)
        }
        (VecInit(res.map(_._1)).asUInt, VecInit(res.map(_._2)).asUInt)
      case sngl if sngl.size == 1 => (sngl.head, 0.U)
      case _ => (0.U, 0.U)
    }}.toSeq
    val (ps, ers) = (bACs.map(_._1), cers ++ bACs.map(_._2))
    if (ps.size == 1) (ps.head, ers) else atc(ps, ers)
  }

  // Compute the initial partial products
  val pprods = (0 until width).map { i =>
    val pprod = VecInit(io.b.asBools.map(_ & io.a(i))).asUInt
    val ext   = 0.U((2*width - pprod.getWidth - i).W)
    if (i == 0) ext ## pprod else ext ## pprod ## 0.U(i.W)
  }

  // Build the approximate tree compressor and compute the error recovery bits
  val (pf, ers) = atc(pprods)
  val ersOr = VecInit((0 until 2*width).map(c => VecInit(ers.map(_(c))).asUInt.orR)).asUInt
  val vs = if (rcvrWidth == 0) 0.U else ersOr(2*width-1, 2*width-rcvrWidth) ## 0.U((2*width-rcvrWidth-1).W)
  io.p := pf + vs
}
