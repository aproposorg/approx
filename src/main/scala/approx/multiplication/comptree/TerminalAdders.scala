package approx.multiplication.comptree

import chisel3._
import chisel3.util._

import approx.util.Xilinx.Common.{genLUT6_2InitString, LUT6_2}
import approx.util.Xilinx.SevenSeries.CARRY4
import approx.util.Xilinx.Versal.{genLUT6CYInitString, LOOKAHEAD8, LUT6CY}

/** Collection of terminal adders useful for compressor tree
 * generation for different devices. We currently include the
 * following device types:
 * - Xilinx FPGAs (denoted by `Xilinx.{SevenSeries, Versal}`)
 */
private[comptree] object TerminalAdders {
  /** Abstract terminal adder class
   * 
   * @param inOps number of input operands
   * @param outW  in-/output bit width
   */
  private[TerminalAdders] abstract class TerminalAdder(inOps: Int, outW: Int) extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(inOps, UInt(outW.W)))
      val out = Output(UInt(outW.W))
    })
  }

  /** Ternary adder for Xilinx 7-Series and UltraScale FPGAs
   * 
   * @param outW in-/output bit width
   * 
   * Implements a ternary adder using LUT6_2 and CARRY4 primitives.
   * The adder takes three input operands of equal bit width and
   * produces a single output sum of the same bit width.
   * 
   * Note, does not implement any form of carry-in.
   */
  class TernaryAdder(outW: Int) extends TerminalAdder(3, outW) {
    // Boolean functions for the LUTs
    val lutFO5 = (ins: Seq[Boolean]) => (ins(1) && ins(2)) || (ins(1) && ins(3)) || (ins(2) && ins(3))
    val lutFO6 = (ins: Seq[Boolean]) => ins.take(4).reduce(_ ^ _)

    // Generate CARRY4 elements
    val nCarry4 = (outW + 3) / 4
    val carries = Seq.fill(nCarry4) { Module(new CARRY4) }
    carries.head.io.CI     := false.B
    carries.head.io.CYINIT := false.B
    (1 until nCarry4).foreach { i =>
      carries(i).io.CI     := carries(i-1).io.CO(3)
      carries(i).io.CYINIT := false.B
    }

    // Generate LUT6_2 elements and connect to CARRY4s
    val tPrimes = Wire(Vec(outW+1, Bool()))
    val luts    = Seq.fill(outW) { Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6))) }
    tPrimes(0) := false.B
    (0 until outW).foreach { i =>
      luts(i).io.I0 := io.in(0)(i)
      luts(i).io.I1 := io.in(1)(i)
      luts(i).io.I2 := io.in(2)(i)
      luts(i).io.I3 := tPrimes(i)
      luts(i).io.I4 := false.B
      luts(i).io.I5 := false.B

      // Connect to CARRY4
      tPrimes(i+1) := luts(i).io.O5
    }
    carries.zipWithIndex.foreach {
      case (carry4, idx) if idx == nCarry4 - 1 && outW % 4 != 0 => // last CARRY4, not fully used
        val usedBits = outW % 4
        carry4.io.S  := VecInit(luts.drop(idx * 4).map(_.io.O6) ++ Seq.fill(4 - usedBits)(false.B)).asUInt
        carry4.io.DI := VecInit(tPrimes.drop(idx * 4) ++ Seq.fill(4 - usedBits)(false.B)).asUInt
      case (carry4, idx) => // fully used CARRY4
        carry4.io.S  := VecInit(luts.drop(idx * 4).take(4).map(_.io.O6)).asUInt
        carry4.io.DI := VecInit(tPrimes.drop(idx * 4).take(4)).asUInt
    }

    // Connect sum outputs
    io.out := VecInit(carries.map(_.io.O).reverse).asUInt(outW-1, 0)
  }

  /** Quaternary adder for Xilinx Versal FPGAs
   * 
   * @param outW in-/output bit width
   * 
   * Implements a quaternary adder using LUT6CY and LOOKAHEAD8 primitives.
   * The adder takes four input operands of equal bit width and
   * produces a single output sum of the same bit width.
   * 
   * Note, does not implement any form of carry-in.
   */
  class QuaternaryAdder(outW: Int) extends TerminalAdder(4, outW) {
    // Helper functions to get LOOKAHEAD8 connections
    def look8CYs(look8: LOOKAHEAD8) = Seq(
      look8.io.CYA, look8.io.CYB, look8.io.CYC, look8.io.CYD,
      look8.io.CYE, look8.io.CYF, look8.io.CYG, look8.io.CYH
    )
    def look8COs(look8: LOOKAHEAD8) = Seq(
      look8.io.CYA, look8.io.COUTB, look8.io.CYC, look8.io.COUTD,
      look8.io.CYE, look8.io.COUTF, look8.io.CYG, look8.io.COUTH
    )
    def look8Props(look8: LOOKAHEAD8) = Seq(
      look8.io.PROPA, look8.io.PROPB, look8.io.PROPC, look8.io.PROPD,
      look8.io.PROPE, look8.io.PROPF, look8.io.PROPG, look8.io.PROPH
    )

    // Generate top and bottom LOOKAHEAD8s
    val nLook8 = (outW + 7) / 8
    val topLook8   = Seq.fill(nLook8) { Module(new LOOKAHEAD8("TRUE", "TRUE", "TRUE", "TRUE")) }
    val topL8CYs   = topLook8.flatMap { look8CYs(_) }
    val topL8COs   = topLook8.flatMap { look8COs(_) }
    val topL8Props = topLook8.flatMap { look8Props(_) }
    topLook8.head.io.CIN := false.B
    topLook8.sliding(2).foreach {
      case Seq(prev, next) => next.io.CIN := prev.io.COUTH
      case _ =>
    }

    val botLook8   = Seq.fill(nLook8) { Module(new LOOKAHEAD8("TRUE", "TRUE", "TRUE", "TRUE")) }
    val botL8CYs   = botLook8.flatMap { look8CYs(_) }
    val botL8COs   = botLook8.flatMap { look8COs(_) }
    val botL8Props = botLook8.flatMap { look8Props(_) }
    botLook8.head.io.CIN := false.B
    botLook8.sliding(2).foreach {
      case Seq(prev, next) => next.io.CIN := prev.io.COUTH
      case _ =>
    }

    // ... default assignments to avoid unconnected wires
    topL8CYs  .foreach(_ := false.B)
    topL8Props.foreach(_ := false.B)

    botL8CYs  .foreach(_ := false.B)
    botL8Props.foreach(_ := false.B)

    // Boolean functions for the LUTs
    val topLutFO51 = (ins: Seq[Boolean]) => ins.take(5).reduce(_ ^ _)
    val topLutFO52 = (ins: Seq[Boolean]) => {
      val s = ins.take(3).reduce(_ ^ _)
      (s && ins(3)) || (s && ins(4)) || (ins(3) && ins(4))
    }

    val botLutFO51 = (ins: Seq[Boolean]) => {
      val c = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
      c ^ ins(3) ^ ins(4)
    }
    val botLutFO52 = (ins: Seq[Boolean]) => {
      val c = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
      (c && ins(3)) || (c && ins(4)) || (ins(3) && ins(4))
    }

    // Generate LUT6CY elements and connect to LOOKAHEAD8s
    val topLuts = Seq.fill(outW) { Module(new LUT6CY(genLUT6CYInitString(topLutFO51, topLutFO52))) }
    val botLuts = Seq.fill(outW) { Module(new LUT6CY(genLUT6CYInitString(botLutFO51, botLutFO52))) }
    (0 until outW).foreach { i =>
      topL8Props(i) := topLuts(i).io.PROP
      botL8Props(i) := botLuts(i).io.PROP

      topL8CYs(i) := topLuts(i).io.O52
      botL8CYs(i) := botLuts(i).io.O52

      if (i > 0) { // carries only from bit 1 onwards
        topLuts(i).io.I4 := topL8COs(i-1)
        botLuts(i).io.I4 := botL8COs(i-1)
      } else {
        topLuts(i).io.I4 := false.B
        botLuts(i).io.I4 := false.B
      }
    }
    topLuts.tail.zip(botLuts).foreach { case (top, bot) => bot.io.I3 := top.io.O51 }
    botLuts.last.io.I3 := topLuts.last.io.O52 // final top-bottom carry

    // Connect data inputs
    (0 until outW).foreach { i =>
      topLuts(i).io.I0 := io.in(0)(i)
      topLuts(i).io.I1 := io.in(1)(i)
      topLuts(i).io.I2 := io.in(2)(i)
      topLuts(i).io.I3 := io.in(3)(i)

      botLuts(i).io.I0 := io.in(0)(i)
      botLuts(i).io.I1 := io.in(1)(i)
      botLuts(i).io.I2 := io.in(2)(i)
    }

    // Connect sum outputs
    io.out := VecInit(botLuts.map(_.io.O51)).asUInt ## topLuts.head.io.O51
  }
}
