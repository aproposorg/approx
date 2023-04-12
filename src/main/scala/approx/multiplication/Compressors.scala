package approx.multiplication

import chisel3._
import chisel3.experimental.RawParam

import approx.addition.{FullAdder, HalfAdder}
import approx.util.Xilinx.Common.{LUT6_2, genLUT6_2InitString}
import approx.util.Xilinx.SevenSeries.CARRY4

/** 1st approximate compressor 4:2
 * 
 * Implementation of design 1 from Momeni et al. [2014]
 */
class Compressor4to2D1 extends C4to2 {
  io.s    := !io.cin & (!(io.x1 ^ io.x2) | !(io.x3 ^ io.x4))
  io.c    := io.cin
  io.cout := !(!(io.x1 & io.x2) | !(io.x3 & io.x4))
}

/** 2nd approximate compressor 4:2
 * 
 * Implementation of design 2 from Momeni et al. [2014<]
 */
class Compressor4to2D2 extends C4to2 {
  io.s    := !(io.x1 ^ io.x2) | !(io.x3 ^ io.x4)
  io.c    := !(!(io.x1 & io.x2) | !(io.x3 & io.x4))
  io.cout := false.B
}

/** Approximate compressor 4:2
 * 
 * Implementation of the compressor from Moaiyeri et al. [2017]
 */
class Compressor4to2Maj extends C4to2 {
  /** Maj function
   * 
   * @param a the first operand
   * @param b the second operand
   * @param c the third operand
   * @return true if a majority of the three inputs are asserted
   */
  def maj(a: Bool, b: Bool, c: Bool) = (a & (b | c)) | (b & c)
  io.s    := maj(io.x1, io.x2, !maj(io.x3, io.x4, !io.cin))
  io.c    := io.x4
  io.cout := io.x3
}

/** Approximate compressor 8:3 IO bundle */
class C8to3IO extends Bundle {
  val x1 = Input(Bool())
  val x2 = Input(Bool())
  val x3 = Input(Bool())
  val x4 = Input(Bool())
  val x5 = Input(Bool())
  val x6 = Input(Bool())
  val x7 = Input(Bool())
  val x8 = Input(Bool())
  val z0  = Output(Bool())
  val z1 = Output(Bool())
  val z2 = Output(Bool())
}

/** Approximate compressor 8:3
 * 
 * Implementation of the compressor from Boroumand and Brisk [2019]
 * 
 * @note This implementation targets Xilinx 7 Series FPGAs. It doesn't 
 *       simulate without primitive code available.
 */
class Compressor8to3Xilinx extends Module {
  val io = IO(new C8to3IO)

  // The design requires one CARRY4 and three LUTs
  val s0 = Wire(Bool())
  val c1 = Wire(Bool())
  val carry = Module(new CARRY4)
  private val lutS0FO5 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2)
  private val lutS0FO6 = (ins: Seq[Boolean]) => false
  val lutS0 = Module(new LUT6_2(genLUT6_2InitString(lutS0FO5, lutS0FO6)))
  private val lutS1FO5 = (ins: Seq[Boolean]) => {
    val or = ins(2) || ins(3)
    (ins(0) && ins(1)) || (ins(0) && or) || (ins(1) && or)
  }
  private val lutS1FO6 = (ins: Seq[Boolean]) => {
    val or = ins(2) || ins(3)
    val s1 = ins(0) ^ ins(1) ^ or
    ins(4) ^ s1
  }
  val lutS1 = Module(new LUT6_2(genLUT6_2InitString(lutS1FO5, lutS1FO6)))
  private val lutC0FO5 = (ins: Seq[Boolean]) => false
  private val lutC0FO6 = (ins: Seq[Boolean]) => {
    val c0 = (ins(1) && ins(2)) || (ins(1) && ins(3)) || (ins(2) && ins(3))
    ins(0) ^ c0
  }
  val lutC0 = Module(new LUT6_2(genLUT6_2InitString(lutC0FO5, lutC0FO6)))
  
  // LUT S0 computes S0 as O5 and false as O6
  // Inputs: (x2, x3, x4, false, false, false)
  lutS0.io.I0 := io.x2
  lutS0.io.I1 := io.x3
  lutS0.io.I2 := io.x4
  lutS0.io.I3 := false.B
  lutS0.io.I4 := false.B
  lutS0.io.I5 := false.B
  s0 := lutS0.io.O5

  // LUT S1 computes C1 as O5 and S1 xor S0 as O6
  // Inputs: (x5, x6, x7, x8, S0, false)
  lutS1.io.I0 := io.x5
  lutS1.io.I1 := io.x6
  lutS1.io.I2 := io.x7
  lutS1.io.I3 := io.x8
  lutS1.io.I4 := s0
  lutS1.io.I5 := false.B
  c1 := lutS1.io.O5

  // LUT C0 computes nothing as O5 and C1 xor C0 as O6
  // Inputs: (c1, x2, x3, x4, false, false)
  lutC0.io.I0 := c1
  lutC0.io.I1 := io.x2
  lutC0.io.I2 := io.x3
  lutC0.io.I3 := io.x4
  lutC0.io.I4 := false.B
  lutC0.io.I5 := false.B

  // The CARRY4 computes the sum bits
  carry.io.CYINIT := false.B
  carry.io.CI     := false.B
  carry.io.DI     := false.B ## c1 ## s0 ## io.x1
  carry.io.S      := false.B ## lutC0.io.O6 ## lutS1.io.O6 ## lutS0.io.O6
  io.z0 := carry.io.O(1)
  io.z1 := carry.io.O(2)
  io.z2 := carry.io.O(3)
}

/** Approximate compressor 8:3
 * 
 * Implementation of the compressor from Boroumand and Brish [2019]
 * 
 * @note This implementation targets Intel FPGAs. For now, this is 
 *       simply implemented with the logic description, not with 
 *       manually instantiated ALMs.
 */
class Compressor8to3Intel extends Module {
  val io = IO(new C8to3IO)
  // The design requires one half adder and three full adders
  val ha  = Module(new HalfAdder)
  val fa1 = Module(new FullAdder)
  val fa2 = Module(new FullAdder)
  val fa3 = Module(new FullAdder)
  // Connect the adders accordingly
  ha.io.x    := io.x1
  ha.io.y    := io.x2
  fa1.io.x   := io.x3
  fa1.io.y   := io.x4
  fa1.io.cin := io.x5
  fa2.io.x   := ha.io.s
  fa2.io.y   := fa1.io.s
  fa2.io.cin := io.x6 | io.x7 | io.x8
  fa3.io.x   := ha.io.cout
  fa3.io.y   := fa1.io.cout
  fa3.io.cin := fa2.io.cout
  io.z0 := fa2.io.s
  io.z1 := fa3.io.s
  io.z2 := fa3.io.cout
}
