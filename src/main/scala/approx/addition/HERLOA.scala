package approx.addition

import chisel3._

/** Hybrid error reduction lower-part OR adder
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the adder from Seo et al. [2020]
 */
class HERLOA(width: Int, val approxWidth: Int) extends Adder(width) {
  require(approxWidth <= width, "width of the approximate part must be less than the width")
  
  val fas = width - approxWidth
  val sums = Wire(Vec(width, Bool()))
  val cins = Wire(Vec(fas + 1, Bool()))
  
  // Generate approximate part
  if (approxWidth == 0) {
    cins(0) := io.cin
  } else if (approxWidth == 1) {
    cins(0) := io.a(approxWidth-1) & io.b(approxWidth-1)
    sums(0) := io.a(approxWidth-1) ^ io.b(approxWidth-1)
  } else {
    cins(0) := io.a(approxWidth-1) & io.b(approxWidth-1)
    val fb = WireDefault(false.B)
    val wn1 = io.a(approxWidth-1) ^ io.b(approxWidth-1)
    val wn2 = io.a(approxWidth-2) & io.b(approxWidth-2)
    fb := wn1 & wn2
    (0 until approxWidth-2).foreach { i =>
      sums(i) := (io.a(i) | io.b(i)) | fb
    }
    sums(approxWidth-2) := (io.a(approxWidth-2) | io.b(approxWidth-2)) & !(!wn1 & wn2)
    sums(approxWidth-1) := wn1 | wn2
  }
  
  // Generate remaining part of the adder
  val adders = Seq.fill(fas) { Module(new FullAdder) }
  (0 until fas).foreach { i =>
    val ind = i + approxWidth
    adders(i).io.x   := io.a(ind)
    adders(i).io.y   := io.b(ind)
    adders(i).io.cin := cins(i)
    sums(ind) := adders(i).io.s
    cins(i+1) := adders(i).io.cout
  }

  // Combine results and output
  io.s := sums.asUInt
  io.cout := cins(fas)
}
