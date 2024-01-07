package approx.multiplication

import chisel3._

/** Error-tolerant multiplier
 * 
 * @param width the width of the multiplier
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the multiplier of Kyaw et al. [2010]
 */
class ETM(width: Int, val approxWidth: Int) extends Multiplier(width, width) {
  require(approxWidth <= width, "width of approximate part must be less than or equal to total width")

  // Approximate the LSBs
  val lowerBits = {
    val v = Wire(Vec(2*approxWidth, Bool()))
    (approxWidth-1 to 0 by -1).foreach { i =>
      if (i == approxWidth-1) {
        v(i+approxWidth) := io.a(i) | io.b(i)
      } else {
        v(i+approxWidth) := io.a(i) | io.b(i) | v(i+approxWidth+1)
      }
      v(i) := v(i+1)
    }
    v.asUInt
  }

  // Instantiate a recursive multiplier for the MSBs
  val mult = Module(new RecursiveMultiplier(width-approxWidth, signed=false))

  // Connect the multiplier and extract the right result
  val ctrl = io.a(width-1, approxWidth).orR | io.b(width-1, approxWidth).orR
  mult.io.a := Mux(ctrl, io.a(width-1, approxWidth), io.b(approxWidth-1, 0))
  mult.io.b := Mux(ctrl, io.b(width-1, approxWidth), io.b(approxWidth-1, 0))
  io.p := Mux(ctrl, mult.io.p ## lowerBits, 0.U(width.W) ## mult.io.p)
}
