package approx.multiplication

import chisel3._

import approx.util.LOPD

/** Approximate dynamic range unbiased multiplier
 * 
 * @param width the width of the multiplier
 * @param segWidth the width of the sub-segments to use in multiplication
 * @param signed whether the operands are signed (defaults to false)
 * 
 * Implementation of the multiplier of Hashemi et al. [2015]
 */
class DRUM(width: Int, segWidth: Int, signed: Boolean = false) extends Multiplier(width, width) {
  require(0 < segWidth && segWidth < width,
    "the sub-segments must be shorter than the full operand width")

  // If the multiplier is signed, change the sign of incoming negative operands
  val (sA, sB) = (io.a(width-1), io.b(width-1))
  val opA = if (!signed) io.a else (VecInit(Seq.fill(width)(sA)).asUInt ^ io.a) + sA
  val opB = if (!signed) io.b else (VecInit(Seq.fill(width)(sB)).asUInt ^ io.b) + sB

  // Treat the operands separately ...
  // ... starting with the multiplier
  val lopdA = Module(new LOPD(width-segWidth+1))
  lopdA.io.in := opA(width-1, segWidth-1)
  val kA = lopdA.io.out
  val mA = Wire(UInt(segWidth.W))
  mA := (opA >> kA) | 1.U

  // ... and continuing with the multiplicand
  val lopdB = Module(new LOPD(width-segWidth+1))
  lopdB.io.in := opB(width-1, segWidth-1)
  val kB = lopdB.io.out
  val mB = Wire(UInt(segWidth.W))
  mB := (opB >> kB) | 1.U

  // Compute the product and shift it to the right position
  val prod = Wire(UInt((2*width).W))
  prod := (mA * mB) << (kA +& kB)

  // If the multiplier is signed, compute the product in the right sign
  val sP = sA ^ sB
  if (!signed) io.p := prod else io.p := (VecInit(Seq.fill(2*width)(sP)).asUInt ^ prod) + sP
}
