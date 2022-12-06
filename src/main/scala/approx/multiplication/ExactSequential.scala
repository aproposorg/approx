package approx.multiplication

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

/** Sequential radix-2 multiplier
 * 
 * @param width the width of the multiplier
 * 
 * Performs multiplication in (at most) width cycles.
 * 
 * Only works for unsigned numbers.
 */
class Radix2SeqMultiplier(width: Int) extends SeqMultiplier(width) {
  /** Calculation states of the multiplier */
  private[Radix2SeqMultiplier] object State extends ChiselEnum {
    val sIdle, sMultiplying, sDone = Value
  }
  import State._

  /** Keep track of the state of the multiplier */
  val a     = Reg(UInt((2*width).W))
  val b     = Reg(UInt(width.W))
  val prod  = Reg(UInt((2*width).W))
  val state = RegInit(sIdle)

  /** Defaut values for all outputs */
  io.in.ready   := false.B
  io.out.valid  := false.B
  io.out.bits.p := prod

  /** Generate calculation and next state logic */
  switch (state) {
    is (sIdle) {
      // Flag that the multiplier is ready to receive inputs
      io.in.ready := true.B

      // If the input values are valid, go to the multiplication state
      when (io.in.valid) {
        a     := 0.U(width.W) ## io.in.bits.a
        b     := io.in.bits.b
        prod  := 0.U
        state := sMultiplying
      }
    }

    is (sMultiplying) {
      // Add another copy of a to the product
      a := a(2*width-2, 0) ## false.B
      b := false.B ## b(width-1, 1)
      prod := Mux(b(0), a, 0.U) + prod

      // Switch to the next state when the number of bits is reached
      when (b === 0.U) {
        state := sDone
      }
    }

    is (sDone) {
      // The output value is valid, wait for the receiver to be ready
      when (io.out.ready) {
        io.out.valid := true.B
        
        when (io.in.valid) {
          a := 0.U(width.W) ## io.in.bits.a
          b := io.in.bits.b
          prod := 0.U
          state := sMultiplying
        }.otherwise {
          state := sIdle
        }
      }
    }
  }
}
