package approx.multiplication

import approx.addition.HOAANED

import chisel3._
import chisel3.util.{is, switch}
import chisel3.ChiselEnum

/** Approximate sequential radix-2 multiplier
 * 
 * @param width the width of the multiplier
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the multiplier of Mannepalli et al. [2021].
 * 
 * Performs multiplication in (at most) width cycles.
 * 
 * Only works for unsigned numbers.
 */
class ApproxRadix2SeqMultiplier(width: Int, val approxWidth: Int) extends SeqMultiplier(width) {
  require(approxWidth <= width,
    "width of the approximate part must be less than or equal to the total width")

  /** Calculation states of the multiplier */
  private[ApproxRadix2SeqMultiplier] object State extends ChiselEnum {
    val sIdle, sMultiplying, sDone = Value
  }
  import State._

  /** Keep track of the state of the multiplier */
  val a     = Reg(UInt((2*width).W))
  val b     = Reg(UInt(width.W))
  val prod  = Reg(UInt((2*width).W))
  val state = RegInit(sIdle)
  val adder = Module(new HOAANED(2*width, approxWidth))
  adder.io.cin := false.B
  adder.io.a   := 0.U
  adder.io.b   := prod

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
      adder.io.a := Mux(b(0), a, 0.U)
      prod := adder.io.s

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
