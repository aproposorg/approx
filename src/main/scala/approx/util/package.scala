package approx

import chisel3._
import chisel3.util.{BitPat, log2Up, MuxCase, Reverse}

package object util {
  /** Leading-one detector
   * 
   * @param width the width of the leading-one detector
   * 
   * Implementation of the LOD from 
   * https://github.com/VLSI-EDA/PoC/blob/master/src/arith/arith_firstone.vhdl#L72-L91
   */
  class LOD(width: Int) extends Module {
    require(width >= 1, "width of leading-one detector must be positive")
    val io = IO(new Bundle {
      val in   = Input(UInt(width.W))
      val zero = Output(Bool())
      val out  = Output(UInt(width.W))
    })
    // Reverse the input vector, invert it, and add one
    val adder  = Reverse(~io.in) +& 1.U
    val oneHot = Reverse(adder(width-1, 0)) & io.in
    io.zero   := adder(width)
    io.out    := oneHot
  }

  /** Leading-one position detector
   * 
   * @param width the width of the leading-one detector
   * 
   * Implementation of the LOPD from 
   * https://github.com/VLSI-EDA/PoC/blob/master/src/arith/arith_firstone.vhdl#L72-L91
   */
  class LOPD(width: Int) extends Module {
    require(width >= 1, "width of leading-one position detector must be positive")
    val io = IO(new Bundle {
      val in   = Input(UInt(width.W))
      val zero = Output(Bool())
      val out  = Output(UInt(log2Up(width).W))
    })
    // Reverse the input vector, invert it, and add one
    val adder  = Reverse(~io.in) +& 1.U
    val oneHot = Reverse(adder(width-1, 0)) & io.in
    io.zero   := adder(width)
    io.out    := VecInit((0 until width).map { i =>
      VecInit(Seq.fill(log2Up(width))(oneHot(i))).asUInt & i.U(log2Up(width).W)
    }).reduceTree(_ | _)
  }

  /** Parallel-read shift register
   * 
   * @param depth the depth of the shift register
   */
  class PRShiftReg[T <: Data](gen: T, depth: Int) extends Module {
    require(depth >= 0, "depth of shift register must be non-negative")
    val io = IO(new Bundle {
      val in  = Input(gen)
      val out = Output(Vec(depth + 1, gen))
    })
    if (depth == 0) {
      io.out(0) := io.in
    } else {
      val regs = Seq.fill(depth) { RegInit(gen, 0.U.asTypeOf(gen)) }
      regs.head := io.in
      (1 until depth).foreach { s =>
        regs(s) := regs(s-1)
      }
      io.out := VecInit(io.in +: regs)
    }
  }
}
