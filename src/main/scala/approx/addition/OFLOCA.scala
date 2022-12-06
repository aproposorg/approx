package approx.addition

import chisel3._

/** Optimized lower-part constant-OR adder
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * @param constWidth the width of the constant part (must be less than or equal to the approximate width)
 * 
 * Implementation of the adder from Dalloo [2018]
 */
class OFLOCA(width: Int, val approxWidth: Int, val constWidth: Int) extends Adder(width) {
  require(approxWidth <= width, "width of the approximate part must be less than or equal to the total width")
  require(constWidth <= approxWidth, "width of the constant part must be less than or equal to the approximate width")
  
  val sums = Wire(Vec(width, Bool()))

  // Generate constant part
  (0 until constWidth).foreach { i => sums(i) := true.B }

  // Generate approximate part
  val fb = Wire(Bool())
  val aNbN = io.a(approxWidth-1) ^ io.b(approxWidth-1)
  (constWidth until approxWidth-1).foreach { i => sums(i) := (io.a(i) | io.b(i)) | fb }
  sums(approxWidth-1) := aNbN

  // Generate remaining part of the adder
  val fas = width - approxWidth
  val adders = Seq.fill(fas) { Module(new FullAdder) }
  val cins = Wire(Vec(fas + 1, Bool()))
  cins(0) := io.a(approxWidth-1) & io.b(approxWidth-1)
  (0 until fas).foreach { i =>
    val ind = i + approxWidth
    adders(i).io.x   := io.a(ind)
    adders(i).io.y   := io.b(ind)
    adders(i).io.cin := cins(i)
    sums(ind) := adders(i).io.s
    cins(i+1) := adders(i).io.cout
  }
  fb := aNbN | cins(fas)

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := cins(fas)
}
