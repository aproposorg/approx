package approx.addition

import chisel3._

/** Carry cut-back approximate adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than the width)
 * @param specWidth the number of bits to use for speculation (must be less than or equal to width / stages)
 * 
 * Implementation of the adder from Camus et al. [2016]
 * 
 * Currently uses a constant false speculation guess.
 */
class CCBA(width: Int, val stages: Int, val specWidth: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")
  require(specWidth <= stageWidth,
    "number of bits used for speculation must be less than width / stages")

  /** Generate a speculative stage carry out
   * 
   * @param p the propagate bits of the operands
   * @param g the generate bits of the operands
   * @param guess a (random) guess of the carry into this block
   * @return a speculative stage carry out
   */
  private[CCBA] def spec(p: UInt, g: UInt, guess: Bool) = {
    val (pOp, gOp) = (p(stageWidth-1, stageWidth-specWidth), g(stageWidth-1, stageWidth-specWidth))
    val couts = Wire(Vec(specWidth+1, Bool()))
    couts(0) := guess
    (1 to specWidth).foreach { i =>
      couts(i) := gOp(i-1) | (pOp(i-1) & couts(i-1))
    }
    couts(specWidth)
  }

  /** Generate a conditional stage carry chain cut
   * 
   * @param p the propagate bits of the operands
   * @return a conditional carry chain cut
   */
  private[CCBA] def prop(p: UInt) = p(stageWidth-1, stageWidth-specWidth).andR()

  val cuts   = Wire(Vec(stages-1, Bool()))
  val specs  = Wire(Vec(stages-1, Bool()))
  val sCouts = Wire(Vec(stages, Bool()))
  val sums   = Wire(Vec(stages, UInt(stageWidth.W)))
  val last   = stages - 1
  (0 until stages).foreach { s =>
    val (top, bot) = ((s+1) * stageWidth - 1, s * stageWidth)
    val (aOp, bOp) = (io.a(top, bot), io.b(top, bot))
    val (p, g) = (aOp ^ bOp, aOp & bOp)
    val cin  = WireDefault(io.cin)
    val sum  = aOp +& bOp + cin
    sums(s) := sum(stageWidth-1, 0)
    s match {
      case 0 => // Insert SPEC only
        specs(s)  := spec(p, g, false.B)
        sCouts(s) := Mux(cuts(s), specs(s), sum(stageWidth))
      case `last` => // Insert PROP only
        cin := sCouts(s-1)
        cuts(s-1) := prop(p)
        sCouts(s) := sum(stageWidth)
      case _ => // Insert both SPEC and PROP
        cin := sCouts(s-1)
        cuts(s-1) := prop(p)
        specs(s)  := spec(p, g, false.B)
        sCouts(s) := Mux(cuts(s), specs(s), sum(stageWidth))
    }
  }

  // Combine results and output
  io.s := sums.asUInt
  io.cout := sCouts(stages-1)
}
