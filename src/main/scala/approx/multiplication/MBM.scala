package approx.multiplication

import chisel3._

import approx.util.LOPD

/** Approximate minimally biased multiplier
 * 
 * @param width the width of the multiplier
 * 
 * Implementation of the multiplier of Saadat et al. [2018]
 * 
 * Only works for unsigned numbers.
 */
class MBM(width: Int) extends Multiplier(width, width) {
  // Treat the operands separately ...
  // ... starting with the multiplier
  val lopdA = Module(new LOPD(width))
  lopdA.io.in := io.a
  val kA = lopdA.io.out
  val fA = (io.a << (width.U - kA))(width-2, 0)

  // ... and continuing with the multiplicand
  val lopdB = Module(new LOPD(width))
  lopdB.io.in := io.b
  val kB = lopdB.io.out
  val fB = (io.b << (width.U - kB))(width-2, 0)

  // Sum the fractional parts
  val fSum = fA +& fB
  // Sum the characteristic parts
  val kSum = (kA +& kB) + fSum(width-1)
  
  // Generate the correction term and add it to the fraction
  private def genCorr(width: Int, curr: String = "b000"): String = if (curr.length < width) {
    genCorr(width, s"$curr${if ((curr.length & 0x1) == 1) "0" else "1"}")
  } else {
    curr
  }
  val c = genCorr(width-1).U
  val fCorr = fSum(width-2, 0) +& Mux(fSum(width-1), c >> 1, c)

  // Append two bits depending on the carry out
  val fFinal = Mux(fCorr(width-1), "b10".U, "b01".U) ## fCorr(width-2, 0)

  // Output the shifted product
  io.p := fFinal << kSum
}
