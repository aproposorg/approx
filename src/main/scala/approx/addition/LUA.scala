package approx.addition

import chisel3._

/** Lu's adder
 * 
 * @param width the width of the adder
 * @param chainWidth the width of each carry chain (must be less than or equal to the width)
 * 
 * Implementation of the adder from Lu [2004]
 */
class LUA(width: Int, val chainWidth: Int) extends Adder(width) {
  require(chainWidth <= width, "the width of the carry chains must be less than or equal to the total width")

  /** Generate a carry select-style chain
   * 
   * @param p the carry propagates
   * @param g the carry generates
   * @return a predicted carry out
   */
  private[LUA] def chain(p: UInt, g: UInt) = {
    require(p.widthKnown && g.widthKnown)
    require(p.getWidth == g.getWidth)
    val couts = Wire(Vec(p.getWidth, Bool()))
    couts(0) := g(0)
    (1 until p.getWidth).foreach { i =>
      couts(i) := g(i) | (p(i) & couts(i-1))
    }
    couts(p.getWidth-1)
  }

  // Generate carries from propagates and generates
  val (p, g)  = (io.a ^ io.b, io.a & io.b)
  val carries = Wire(Vec(width+1, Bool()))
  carries(0) := io.cin
  (1 to width).foreach { i =>
    val (top, bot) = (i-1, if (i-chainWidth < 0) 0 else i-chainWidth)
    carries(i) := chain(p(top, bot), g(top, bot))
  }

  // Combine results and output
  io.s    := p ^ carries.asUInt()(width-1, 0)
  io.cout := carries(width)
}
