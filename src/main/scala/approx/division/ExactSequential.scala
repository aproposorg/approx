package approx.division

import chisel3._
import chisel3.util.log2Up

/** Radix 2 sequential integer divider
 * 
 * @param width the width of the divider
 * 
 * Implementation of the algorithm from 
 * https://projectf.io/posts/division-in-verilog/ [2023]
 * 
 * Only works for unsigned numbers.
 */
class Radix2SeqDivider(width: Int) extends Divider(width) {
  // Internal registers for keeping track of quotient and remainder
  val bInt = Reg(UInt(width.W))
  val quo  = Reg(UInt(width.W))
  val acc  = Reg(UInt((width+1).W))
  val iter = Reg(UInt(log2Up(width).W))

  // Next state signals
  val accNext = Wire(chiselTypeOf(acc))
  val quoNext = Wire(chiselTypeOf(quo))
  when(acc >= (false.B ## bInt)) {
    accNext := (acc - bInt)(width-1, 0) ## quo(width-1)
    quoNext := quo(width-2, 0) ## true.B
  }.otherwise {
    accNext := acc(width-1, 0) ## quo(width-1)
    quoNext := quo(width-2, 0) ## false.B
  }

  // Registers for various flags
  val busy = RegInit(false.B)
  val done = RegInit(false.B)
  val dbz  = RegInit(false.B)

  // Create the data path in parallel with the control signals
  when(io.start) {
    done := false.B
    iter := 0.U
    when(!io.b.orR) { // catch division by zero
      busy := false.B
      done := true.B
      dbz  := true.B
    }.otherwise { // initialize computation
      busy := true.B
      dbz  := false.B
      bInt := io.b
      acc  := 0.U(width.W) ## io.a(width-1)
      quo  := io.a(width-2, 0) ## false.B
    }
  }.elsewhen(busy) {
    when(iter === (width-1).U) {
      busy := false.B
      done := true.B
      acc  := false.B ## accNext(width, 1)
      quo  := quoNext
    }.otherwise {
      iter := iter + 1.U
      acc  := accNext
      quo  := quoNext
    }
  }

  // Connect the registers to the IO
  io.q    := quo
  io.r    := acc
  io.busy := busy
  io.done := done
  io.dbz  := dbz
}
