package approx.addition

import chisel3._

/** Hardware optimized approximate adder with normal error distribution
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the adder from Balasubramian et al. [2020]
 */
class HOAANED(width: Int, val approxWidth: Int) extends Adder(width) {
  require(approxWidth <= width, "width of the approximate part must be less than or equal to the total width")
  val sums = Wire(Vec(width, Bool()))

  // Generate constant part
  val cin = Wire(Bool())
  if (approxWidth == 0) {
    // No constant part - pass the carry-in directly
    cin := io.cin
  } else if (approxWidth == 1) {
    // 1-bit constant part
    sums(0) := io.a(0) | io.b(0)
    cin     := io.a(0) & io.b(0)
  } else {
    // n-bit constant part (n >= 2)
    (0 until approxWidth-2).foreach { i => sums(i) := true.B }
    sums(approxWidth-2) := io.a(approxWidth-2) | io.b(approxWidth-2)
    sums(approxWidth-1) := Mux(cin, false.B, io.a(approxWidth-1) | io.b(approxWidth-1)) | (io.a(approxWidth-2) & io.b(approxWidth-2))
    cin := io.a(approxWidth-1) & io.b(approxWidth-1)
  }

  // Generate remaining part of the adder
  val rca     = Module(new RCA(width - approxWidth))
  rca.io.a   := io.a(width-1, approxWidth)
  rca.io.b   := io.b(width-1, approxWidth)
  rca.io.cin := cin
  (approxWidth until width).foreach { i => sums(i) := rca.io.s(i-approxWidth) }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := rca.io.cout
}
