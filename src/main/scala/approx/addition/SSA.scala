package approx.addition

import chisel3._
import chisel3.util._

/** Static segment adder
 * 
 * @param width the width of the adder (must be a power of 2 and greater than or equal to 4)
 * 
 * Hardcoded 4-segment implementation of the adder from Jothin and Vasanthanayaki [2017]
 */
class SSA(width: Int) extends Adder(width) {
  val stageWidth = width / 4
  require(isPow2(width), "width must be a power of 2")
  require(width >= 4, "width must be greater than or equal to 4")

  // Find the top segment
  val aVec = Wire(Vec(4, UInt(stageWidth.W)))
  aVec := io.a.asTypeOf(aVec)
  val bVec = Wire(Vec(4, UInt(stageWidth.W)))
  bVec := io.b.asTypeOf(bVec)
  val aORs = Wire(Vec(4, Bool()))
  aORs := VecInit(aVec.map { _.orR })
  val bORs = Wire(Vec(4, Bool()))
  bORs := VecInit(bVec.map { _.orR })
  val cORs = Wire(UInt(4.W))
  cORs := VecInit(aVec.zip(bVec).map { case (a, b) => a | b }).asUInt

  // Perform the addition and output results
  val op1 = WireDefault(aVec(0))
  val op2 = WireDefault(bVec(0))
  val cin = WireDefault(io.cin)
  val adder = op1 +& op2 + cin
  val sums = Wire(Vec(4, UInt(stageWidth.W)))
  sums := adder.asTypeOf(sums)
  val cout = WireDefault(false.B)
  when(cORs === BitPat("b1???")) {
    // Set operands
    op1 := aVec(3)
    op2 := bVec(3)
    cin := aVec(2)(stageWidth-1) & bVec(2)(stageWidth-1)
    // Update sum and carry out
    sums := (adder(stageWidth-1, 0) ## (aVec(2)(stageWidth-1, stageWidth-2) | bVec(2)(stageWidth-1, stageWidth-2)) ## s"b${"1"*(3*stageWidth-2)}".U).asTypeOf(sums)
    cout := adder(stageWidth)
  }.elsewhen(cORs === BitPat("b01??")) {
    // Set up operands
    op1 := aVec(2)
    op2 := bVec(2)
    cin := aVec(1)(stageWidth-1) & bVec(1)(stageWidth-1)
    // Update sum and carry out
    sums := (adder ## (aVec(1)(stageWidth-1, stageWidth-2) | bVec(1)(stageWidth-1, stageWidth-2)) ## s"b${"1"*(2*stageWidth-2)}".U).asTypeOf(sums)
  }.elsewhen(cORs === BitPat("b001?")) {
    // Set up operands
    op1 := aVec(1)
    op2 := bVec(1)
    cin := aVec(0)(stageWidth-1) & bVec(0)(stageWidth-1)
    // Update sum and carry out
    sums := (adder ## (aVec(0)(stageWidth-1, stageWidth-2) | bVec(0)(stageWidth-1, stageWidth-2)) ## s"b${"1"*(stageWidth-2)}".U).asTypeOf(sums)
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := cout
}
