package approx.addition

import chisel3._

/** Lower-part constant-OR adder
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * @param constWidth the width of the constant part (must be less than or equal to the approximate width)
 * 
 * Implementation of the adder from Dalloo [2018]
 */
class LOCA(width: Int, val approxWidth: Int, val constWidth: Int) extends Adder(width) {
  require(approxWidth <= width,
    "width of the approximate part must be less than or equal to the total width")
  require(constWidth <= approxWidth,
    "width of the constant part must be less than or equal to the approximate width")
  
  val sums = Wire(Vec(width, Bool()))

  // Generate constant part
  (0 until constWidth).foreach { i => sums(i) := true.B }

  // Generate approximate part
  (constWidth until approxWidth).foreach { i => sums(i) := io.a(i) | io.b(i) }

  // Generate remaining part of the adder
  val fas = width - approxWidth
  val adders = Seq.fill(fas) { Module(new FullAdder) }
  val cins = Wire(Vec(fas + 1, Bool()))
  cins(0) := false.B
  (0 until fas).foreach { i =>
    val ind = i + approxWidth
    adders(i).io.x   := io.a(ind)
    adders(i).io.y   := io.b(ind)
    adders(i).io.cin := cins(i)
    sums(ind) := adders(i).io.s
    cins(i+1) := adders(i).io.cout
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := cins(fas)
}
