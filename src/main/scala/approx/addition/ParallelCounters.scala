package approx.addition

import chisel3._

/** Collection of useful parallel counter implementations 
 * 
 * @note Currently includes the following device types
 *       - Xilinx devices
 */
object ParallelCounters {
  /** Useful parallel counter implementations for Xilinx devices
   * 
   * @note Currently includes the following atoms
   *       - (2,2)
   *       - (1,4)
   *       - (0,6)
   *       - (2,5 : 1,2,1]
   *       - Ternary adder atom
   * 
   * @note Implementations of the atoms from Preusser [2018]
   */
  object Xilinx {
    import approx.util.Xilinx.Common.{genLUT6InitString, genLUT6_2InitString, LUT6, LUT6_2}

    class Atom22 extends Module {
      val io = IO(new Bundle {
        val a0 = Input(Bool())
        val a1 = Input(Bool())
        val b0 = Input(Bool())
        val b1 = Input(Bool())
        val c  = Input(Bool())
        val s0 = Output(Bool())
        val s1 = Output(Bool())
        val cO = Output(Bool())
      })
      private val lutFO5 = (ins: Seq[Boolean]) => ins(1)
      private val lutFO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1)
      // Inputs: (a0, a1, false, false, false, false)
      val lut0 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
      lut0.io.I0 := io.a0
      lut0.io.I1 := io.a1
      lut0.io.I2 := false.B
      lut0.io.I3 := false.B
      lut0.io.I4 := false.B
      lut0.io.I5 := false.B
      // Inputs: (b0, b1, false, false, false, false)
      val lut1 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
      lut1.io.I0 := io.b0
      lut1.io.I1 := io.b1
      lut1.io.I2 := false.B
      lut1.io.I3 := false.B
      lut1.io.I4 := false.B
      lut1.io.I5 := false.B
      val cin = Mux(lut0.io.O6, io.c, lut0.io.O5)
      io.cO  := Mux(lut1.io.O6, cin,  lut1.io.O5)
      io.s0  := lut0.io.O6 ^ io.c
      io.s1  := lut1.io.O6 ^ cin
    }

    class Atom14 extends Module {
      val io = IO(new Bundle {
        val a0 = Input(Bool())
        val a1 = Input(Bool())
        val a2 = Input(Bool())
        val a3 = Input(Bool())
        val b0 = Input(Bool())
        val c  = Input(Bool())
        val s0 = Output(Bool())
        val s1 = Output(Bool())
        val cO = Output(Bool())
      })
      private val lutFO5  = (ins: Seq[Boolean]) => ins(3)
      private val lut0FO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2) ^ ins(3)
      private val lut1FO6 = (ins: Seq[Boolean]) => ((ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))) ^ ins(3)
      // Inputs: (a0, a1, a2, a3, false, false)
      val lut0 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lut0FO6)))
      lut0.io.I0 := io.a0
      lut0.io.I1 := io.a1
      lut0.io.I2 := io.a2
      lut0.io.I3 := io.a3
      lut0.io.I4 := false.B
      lut0.io.I5 := false.B
      // Inputs: (a0, a1, a2, b1, false, false)
      val lut1 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lut1FO6)))
      lut1.io.I0 := io.a0
      lut1.io.I1 := io.a1
      lut1.io.I2 := io.a2
      lut1.io.I3 := io.b0
      lut1.io.I4 := false.B
      lut1.io.I5 := false.B
      val cin = Mux(lut0.io.O6, io.c, lut0.io.O5)
      io.cO  := Mux(lut1.io.O6, cin,  lut1.io.O5)
      io.s0  := lut0.io.O6 ^ io.c
      io.s1  := lut1.io.O6 ^ cin
    }

    class Atom06 extends Module {
      val io = IO(new Bundle {
        val a0 = Input(Bool())
        val a1 = Input(Bool())
        val a2 = Input(Bool())
        val a3 = Input(Bool())
        val a4 = Input(Bool())
        val a5 = Input(Bool())
        val c  = Input(Bool())
        val s0 = Output(Bool())
        val s1 = Output(Bool())
        val cO = Output(Bool())
      })
      private val lut0FO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2) ^ ins(3) ^ ins(4) ^ ins(5)
      private val lut1FO5 = (ins: Seq[Boolean]) => (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
      private val lut1FO6 = (ins: Seq[Boolean]) => {
        val s0 = ins(0) ^ ins(1) ^ ins(2)
        val c0 = (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
        val c1 = (s0 & ins(3)) | (s0 & ins(4)) | (ins(3) & ins(4))
        c0 ^ c1
      }
      // Inputs: (a0, a1, a2, a3, a4, a5)
      val lut0 = Module(new LUT6(genLUT6InitString(lut0FO6)))
      lut0.io.I0 := io.a0
      lut0.io.I1 := io.a1
      lut0.io.I2 := io.a2
      lut0.io.I3 := io.a3
      lut0.io.I4 := io.a4
      lut0.io.I5 := io.a5
      // Inputs: (a0, a1, a2, a3, a4, false)
      val lut1 = Module(new LUT6_2(genLUT6_2InitString(lut1FO5, lut1FO6)))
      lut1.io.I0 := io.a0
      lut1.io.I1 := io.a1
      lut1.io.I2 := io.a2
      lut1.io.I3 := io.a3
      lut1.io.I4 := io.a4
      lut1.io.I5 := false.B
      val cin = Mux(lut0.io.O, io.c, io.a5)
      io.cO  := Mux(lut1.io.O6, cin,  lut1.io.O5)
      io.s0  := lut0.io.O  ^ io.c
      io.s1  := lut1.io.O6 ^ cin
    }

    class Atom25 extends Module {
      val io = IO(new Bundle {
        val a0 = Input(Bool())
        val a1 = Input(Bool())
        val a2 = Input(Bool())
        val a3 = Input(Bool())
        val a4 = Input(Bool())
        val b0 = Input(Bool())
        val b1 = Input(Bool())
        val s0 = Output(Bool())
        val s1 = Output(Bool())
        val c0 = Output(Bool())
        val c1 = Output(Bool())
      })
      private val lutLOFO5 = (ins: Seq[Boolean]) => {
        val sint = ins(2) ^ ins(3) ^ ins(4)
        (ins(0) & ins(1)) | (ins(0) & sint) | (ins(1) & sint)
      }
      private val lutLOFO6 = (ins: Seq[Boolean]) => {
        val sint = ins(2) ^ ins(3) ^ ins(4)
        ins(0) ^ ins(1) ^ sint
      }
      private val lutHIFO5 = (ins: Seq[Boolean]) => {
        val cint = (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
        (cint & ins(3)) | (cint & ins(4)) | (ins(3) & ins(4))
      }
      private val lutHIFO6 = (ins: Seq[Boolean]) => {
        val cint = (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
        cint ^ ins(3) ^ ins(4)
      }
      // Inputs: (a0, a1, a2, a3, a4, false)
      val lutLO = Module(new LUT6_2(genLUT6_2InitString(lutLOFO5, lutLOFO6)))
      lutLO.io.I0 := io.a0
      lutLO.io.I1 := io.a1
      lutLO.io.I2 := io.a2
      lutLO.io.I3 := io.a3
      lutLO.io.I4 := io.a4
      lutLO.io.I5 := false.B
      // Inputs: (a2, a3, a4, b0, b1, false)
      val lutHI = Module(new LUT6_2(genLUT6_2InitString(lutHIFO5, lutHIFO6)))
      lutHI.io.I0 := io.a2
      lutHI.io.I1 := io.a3
      lutHI.io.I2 := io.a4
      lutHI.io.I3 := io.b0
      lutHI.io.I4 := io.b1
      lutHI.io.I5 := false.B
      io.c0 := lutLO.io.O5
      io.s0 := lutLO.io.O6
      io.c1 := lutHI.io.O5
      io.s1 := lutHI.io.O6
    }

    // @todo fix this
    class Ternary extends Module {
      val io = IO(new Bundle {
        val a0 = Input(Bool())
        val a1 = Input(Bool())
        val a2 = Input(Bool())
        val z  = Input(Bool())
        val c  = Input(Bool())
        val s  = Output(Bool())
        val zO = Output(Bool())
        val cO = Output(Bool())
      })
      private val lutFO5 = (ins: Seq[Boolean]) => (ins(1) & ins(2)) | (ins(1) & ins(3)) | (ins(2) & ins(3))
      private val lutFO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2) ^ ins(3)
      // Inputs: (z, a0, a1, a2, false, false)
      val lut = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
      lut.io.I0 := io.z
      lut.io.I1 := io.a0
      lut.io.I2 := io.a1
      lut.io.I3 := io.a2
      lut.io.I4 := false.B
      lut.io.I5 := false.B
      io.zO  := lut.io.O5
      io.cO  := Mux(lut.io.O6, io.c, io.z)
      io.s   := lut.io.O6 ^ io.c
    }
  }
}

object ParallelCountersTest extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ParallelCounters.Xilinx.Atom22, Array("--target-dir", "build"))
  (new chisel3.stage.ChiselStage).emitVerilog(new ParallelCounters.Xilinx.Atom14, Array("--target-dir", "build"))
  (new chisel3.stage.ChiselStage).emitVerilog(new ParallelCounters.Xilinx.Atom06, Array("--target-dir", "build"))
  (new chisel3.stage.ChiselStage).emitVerilog(new ParallelCounters.Xilinx.Atom25, Array("--target-dir", "build"))
  (new chisel3.stage.ChiselStage).emitVerilog(new ParallelCounters.Xilinx.Ternary, Array("--target-dir", "build"))
}
