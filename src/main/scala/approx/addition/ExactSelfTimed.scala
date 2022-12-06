package approx.addition

import chisel3._

/** Exact self-timed adder
 * 
 * @param width the width of the adder
 * 
 * Implementation of the Type I STA from Ercegovac and Lang [2004]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class STA(width: Int) extends SelfTimedAdder(width) {
  /** Self-timed full-adder circuit
   * 
   * @param x the first operand
   * @param y the second operand
   * @param c0I the lower double-rail carry in
   * @param c1I the upper double-rail carry in
   * @return a triple of (sum bit, lower double-rail carry out, upper double-rail carry out)
   */
  private[STA] def stfa(x: Bool, y: Bool, c0I: Bool, c1I: Bool) = {
    val (k, g, p) = (!x & !y, x & y, x ^ y)
    val (s, c0O, c1O) = (WireDefault(false.B), WireDefault(false.B), WireDefault(false.B))
    s   := p ^ c1I
    c0O := (k & c1I) | ((p | k) & c0I)
    c1O := (g & c0I) | ((p | g) & c1I)
    (s, c0O, c1O)
  }

  // Generate all the intermediate signals
  val sums = Wire(Vec(width, Bool()))
  val c0s  = Wire(Vec(width+1, Bool()))
  val c1s  = Wire(Vec(width+1, Bool()))
  c0s(0)  := !io.cin
  c1s(0)  := io.cin
  (0 until width).foreach { i =>
    val fa = stfa(io.a(i), io.b(i), c0s(i), c1s(i))
    sums(i)  := fa._1
    c0s(i+1) := fa._2
    c1s(i+1) := fa._3
  }
  
  // Combine results and output
  io.s    := sums.asUInt
  io.cout := c1s(width)
  io.f    := c0s(width) | c1s(width)
}

/** Exact parallel carry completion sensing adder
 * 
 * @param width the width of the adder
 * 
 * Implementation of the Type II CCA from Ercegovac and Lang [2004]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class CCA(width: Int) extends SelfTimedAdder(width) {
  /** Parallel carry self-timed full-adder circuit
   * 
   * @param x the first operand
   * @param y the second operand
   * @param c0I the lower double-rail carry in
   * @param c1I the upper double-rail carry in
   * @return a triple of (sum bit, lower double-rail carry out, upper double-rail carry out)
   */
  private[CCA] def csfa(x: Bool, y: Bool, c0I: Bool, c1I: Bool) = {
    val (k, g, p) = (!x & !y, x & y, x ^ y)
    val (s, c0O, c1O) = (WireDefault(false.B), WireDefault(false.B), WireDefault(false.B))
    s   := p ^ c1I
    c0O := k | (p & c0I)
    c1O := g | (p & c1I)
    (s, c0O, c1O)
  }

  // Generate all the intermediate signals
  val sums = Wire(Vec(width, Bool()))
  val c0s  = Wire(Vec(width+1, Bool()))
  val c1s  = Wire(Vec(width+1, Bool()))
  c0s(0)  := !io.cin
  c1s(0)  := io.cin
  (0 until width).foreach { i =>
    val fa = csfa(io.a(i), io.b(i), c0s(i), c1s(i))
    sums(i)  := fa._1
    c0s(i+1) := fa._2
    c1s(i+1) := fa._3
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := c1s(width)
  io.f    := (c0s.asUInt | c1s.asUInt).andR
}
