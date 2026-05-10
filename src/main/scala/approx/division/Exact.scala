package approx.division

import chisel3._
import chisel3.util._

/** Radix-2 combinational divider using digit recurrence
 *
 * @param width the width of the divider
 */
class Radix2Divider(width: Int) extends Divider(width) {
  private val zeroDiv = io.b === 0.U

  val qVec = Wire(Vec(width, Bool()))
  val rVec = Wire(Vec(width + 1, UInt((width + 1).W)))
  rVec(0) := 0.U

  for (stage <- 0 until width) {
    val div     = io.a(width - 1 - stage)
    val shifted = rVec(stage)(width - 1, 0) ## div
    val take    = shifted >= io.b
    qVec(stage) := take
    rVec(stage + 1) := Mux(take, shifted - io.b, shifted)
  }

  // Quotient bits are in MSB-first order
  io.q := Mux(zeroDiv, 0.U, VecInit(qVec.reverse).asUInt)
  io.r := Mux(zeroDiv, io.a, rVec(width))
}

/** Radix-4 combinational divider using digit recurrence
 *
 * @param width the width of the divider
 */
class Radix4Divider(width: Int) extends Divider(width) {
  private val zeroDiv  = io.b === 0.U
  private val padWidth = width + (width & 1)
  private val stages   = padWidth / 2

  // Pad the divident to an integer multiple of 2 bits
  private val aPad = if (padWidth == width) io.a else false.B ## io.a

  // Extend the divisor and compute 2x and 3x multiples for comparison
  private val bExt   = 0.U(4.W) ## io.b
  private val bExtX2 = bExt << 1
  private val bExtX3 = bExt + bExtX2

  val qVec = Wire(Vec(stages, UInt(2.W)))
  val rVec = Wire(Vec(stages + 1, UInt((width + 2).W)))
  rVec(0) := 0.U

  for (stage <- 0 until stages) {
    val bitHigh = aPad(padWidth - 1 - stage * 2)
    val bitLow  = aPad(padWidth - 2 - stage * 2)
    val shifted = Cat(rVec(stage), bitHigh, bitLow)

    when(shifted >= bExtX3) {
      qVec(stage)     := 3.U
      rVec(stage + 1) := shifted - bExtX3
    }.elsewhen(shifted >= bExtX2) {
      qVec(stage)     := 2.U
      rVec(stage + 1) := shifted - bExtX2
    }.elsewhen(shifted >= bExt) {
      qVec(stage)     := 1.U
      rVec(stage + 1) := shifted - bExt
    }.otherwise {
      qVec(stage)     := 0.U
      rVec(stage + 1) := shifted
    }
  }

  // Quotient digits are in MSB-first order
  val fullQ = VecInit(qVec.reverse).asUInt
  io.q := Mux(zeroDiv, 0.U, fullQ(width - 1, 0))
  io.r := Mux(zeroDiv, io.a, rVec(stages)(width - 1, 0))
}
