package approx

import chisel3._
import chisel3.util.{BitPat, log2Up, MuxCase, Reverse}

package object util {
  /** Leading-one detector
   * @param width the width of the leading-one detector
   * 
   * @note Implementation of the LOD from 
   *       https://github.com/VLSI-EDA/PoC/blob/master/src/arith/arith_firstone.vhdl#L72-L91
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
   * @param width the width of the leading-one detector
   * 
   * @note Implementation of the LOPD from 
   *       https://github.com/VLSI-EDA/PoC/blob/master/src/arith/arith_firstone.vhdl#L72-L91
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
}
