package approx.multiplication

import chisel3._

import approx.addition.{FullAdder, HalfAdder}
import approx.util.Xilinx.Common.{LUT6_2, genLUT6_2InitString}
import approx.util.Xilinx.SevenSeries.CARRY4
import approx.util.Xilinx.Versal.{LUT6CY, genLUT6CYInitString}

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
 * Implementation of design 2 from Momeni et al. [2014]
 */
class Compressor4to2D2 extends C4to2 {
  io.s    := !(io.x1 ^ io.x2) | !(io.x3 ^ io.x4)
  io.c    := !(!(io.x1 & io.x2) | !(io.x3 & io.x4))
  io.cout := false.B
}

/** 1st approximate compressor 4:2
 * 
 * Implementation of design CV1 from Zanandrea and Meinhardt [2023]
 * 
 * Should have the cin input held low constantly.
 */
class Compressor4to2CV1 extends C4to2 {
  io.s    := io.x2
  io.c    := io.x3 | io.x4
  io.cout := false.B
}

/** 2nd approximate compressor 4:2
 * 
 * Implementation of design CV2 from Zanandrea and Meinhardt [2023]
 * 
 * Should have the cin input held low constantly.
 */
class Compressor4to2CV2 extends C4to2 {
  io.s    := true.B
  io.c    := io.x4
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

/** Abstract approximate 8:3 compressor module class */
abstract class C8to3 extends Module {
  val io = IO(new C8to3IO)
}

/** Approximate compressor 8:3
 * 
 * Implementation of the compressor from Boroumand and Brisk [2019]
 * 
 * This implementation targets Xilinx 7 Series and UltraScale
 * FPGAs. It doesn't simulate without primitive code available.
 */
class Compressor8to3SevenSeries extends C8to3 {
  // The design requires one CARRY4 and three LUTs
  val s0 = Wire(Bool())
  val c1 = Wire(Bool())
  val carry = Module(new CARRY4)
  
  // LUT S0 computes S0 as O5 and false as O6
  // Inputs: (x2, x3, x4, false, false, true)
  private val lutS0FO5 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2)
  private val lutS0FO6 = (ins: Seq[Boolean]) => false
  val lutS0 = Module(new LUT6_2(genLUT6_2InitString(lutS0FO5, lutS0FO6)))
  lutS0.io.I0 := io.x2
  lutS0.io.I1 := io.x3
  lutS0.io.I2 := io.x4
  lutS0.io.I3 := false.B
  lutS0.io.I4 := false.B
  lutS0.io.I5 := true.B
  s0 := lutS0.io.O5

  // LUT S1 computes C1 as O5 and S1 xor S0 as O6
  // Inputs: (x5, x6, x7, x8, S0, true)
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
  lutS1.io.I0 := io.x5
  lutS1.io.I1 := io.x6
  lutS1.io.I2 := io.x7
  lutS1.io.I3 := io.x8
  lutS1.io.I4 := s0
  lutS1.io.I5 := true.B
  c1 := lutS1.io.O5

  // LUT C0 computes nothing as O5 and C1 xor C0 as O6
  // Inputs: (c1, x2, x3, x4, false, true)
  private val lutC0FO5 = (ins: Seq[Boolean]) => false
  private val lutC0FO6 = (ins: Seq[Boolean]) => {
    val c0 = (ins(1) && ins(2)) || (ins(1) && ins(3)) || (ins(2) && ins(3))
    ins(0) ^ c0
  }
  val lutC0 = Module(new LUT6_2(genLUT6_2InitString(lutC0FO5, lutC0FO6)))
  lutC0.io.I0 := c1
  lutC0.io.I1 := io.x2
  lutC0.io.I2 := io.x3
  lutC0.io.I3 := io.x4
  lutC0.io.I4 := false.B
  lutC0.io.I5 := true.B

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
 * Implementation of the compressor from Boroumand and Brisk [2019]
 * 
 * This implementation targets AMD Versal FPGAs. It doesn't simulate
 * without primitive code available.
 * 
 * Beware that Vivado may not map the three instantiated LUTs consecutively
 * along their cascade signals without the user manually adding `RLOC` and
 * `HU_SET` attributes to the LUT instances.
 */
class Compressor8to3Versal extends C8to3 {
  // The design requires three LUTs
  val c1 = Wire(Bool())
  val s1 = Wire(Bool())
  val c2 = Wire(Bool())

  // LUT S1 computes C1 as O51 and S1 as O52
  // Inputs: (x5, x6, x7, x8, false, true)
  private val lutS1FO51 = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && (ins(2) || ins(3))) || (ins(1) && (ins(2) || ins(3)))
  private val lutS1FO52 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ (ins(2) || ins(3))
  val lutS1 = Module(new LUT6CY(genLUT6CYInitString(lutS1FO51, lutS1FO52)))
  lutS1.io.I0 := io.x5
  lutS1.io.I1 := io.x6
  lutS1.io.I2 := io.x7
  lutS1.io.I3 := io.x8
  lutS1.io.I4 := false.B
  c1 := lutS1.io.O51
  s1 := lutS1.io.O52

  // LUT C2 computes Z0 and O51 and C2 as O52
  // Inputs: (x1, x2, x3, x4, s1, true)
  private val lutC2FO51 = (ins: Seq[Boolean]) => {
    val s0 = ins(1) ^ ins(2) ^ ins(3)
    ins(0) ^ s0 ^ ins(4)
  }
  private val lutC2FO52 = (ins: Seq[Boolean]) => {
    val s0 = ins(1) ^ ins(2) ^ ins(3)
    (ins(0) && s0) || (ins(0) && ins(4)) || (s0 && ins(4))
  }
  val lutC2 = Module(new LUT6CY(genLUT6CYInitString(lutC2FO51, lutC2FO52)))
  lutC2.io.I0 := io.x1
  lutC2.io.I1 := io.x2
  lutC2.io.I2 := io.x3
  lutC2.io.I3 := io.x4
  lutC2.io.I4 := s1
  io.z0 := lutC2.io.O51
  c2    := lutC2.io.O52

  // LUT Z computes Z1 as O51 and Z2 as O52
  // Inputs: (x2, x3, x4, c1, c2, true)
  private val lutZFO51  = (ins: Seq[Boolean]) => {
    val c0 = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
    c0 ^ ins(3) ^ ins(4)
  }
  private val lutZFO52  = (ins: Seq[Boolean]) => {
    val c0 = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
    (c0 && ins(3)) || (c0 && ins(4)) || (ins(3) && ins(4))
  }
  val lutZ  = Module(new LUT6CY(genLUT6CYInitString(lutZFO51, lutZFO52)))
  lutZ.io.I0 := io.x2
  lutZ.io.I1 := io.x3
  lutZ.io.I2 := io.x4
  lutZ.io.I3 := c1
  lutZ.io.I4 := c2
  io.z1 := lutZ.io.O51
  io.z2 := lutZ.io.O52
}

/** Approximate compressor 8:3
 * 
 * Implementation of the compressor from Boroumand and Brish [2019]
 * 
 * This implementation targets Intel FPGAs and ASICs. As Intel flows do
 * not support manual primitive instantiation, we keep the RTL description.
 */
class Compressor8to3 extends C8to3 {
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
