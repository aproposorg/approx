package approx.multiplication

import chisel3._

/** Approximate low-power small-area multiplier
 * 
 * @param width the width of the multiplier
 * @param truncWidth the width of the truncated part
 * 
 * Implementation of the multiplier of Baba et al. [2018]
 * 
 * Only works for unsigned numbers. Excludes the carry-maskable adder.
 */
class LPSA(width: Int, truncWidth: Int) extends Multiplier(width, width) {
  require(truncWidth < 2*width,
    "the truncated part must be shorter than the product width")

  /** Recursively build an approximate tree compressor with incomplete adder cells
   * 
   * @param cps the current partial products
   * @param cqs the current approximate carries
   * @return a single row of compressed sum bits and a list of approximate carry rows
   */
  private[multiplication] def atc(cps: Seq[UInt], cqs: Seq[UInt] = Seq.empty[UInt]): (UInt, Seq[UInt]) = {
    val iCACs = cps.sliding(2, 2).map { _ match {
      case pr if pr.size == 2 => (pr.head | pr.last, pr.head & pr.last)
      case sngl if sngl.size == 1 => (sngl.head, 0.U)
      case _ => (0.U, 0.U)
    }}.toSeq
    val (ps, qs) = (iCACs.map(_._1), cqs ++ iCACs.map(_._2))
    if (ps.size == 1) (ps.head, qs) else atc(ps, qs)
  }

  // Compute the initial partial products
  val pprods = (0 until width).map { i =>
    val pprod = VecInit(io.b.asBools.map(_ & io.a(i))).asUInt
    val ext   = 0.U((2*width - pprod.getWidth - i).W)
    if (i == 0) ext ## pprod else ext ## pprod ## 0.U(i.W)
  }

  // Build the approximate tree compressor and compute the carries
  val (pf, qs) = atc(pprods)
  val qsOr = VecInit((0 until 2*width).map(c => VecInit(qs.map(_(c))).asUInt.orR)).asUInt
  val vs = if (truncWidth == 0) qsOr else qsOr(2*width-1, truncWidth) ## 0.U(truncWidth.W)
  io.p := pf + vs
}
