package approx

import chisel3._
import chisel3.util.{BitPat, log2Up, MuxCase}

package object util {
  /** Leading-one detector
   * @param width the width of the leading-one detector
   */
  class LOD(width: Int) extends Module {
    require(width >= 1, "width of leading-one detector must be positive")
    val io = IO(new Bundle {
      val in   = Input(UInt(width.W))
      val zero = Output(Bool())
      val out  = Output(UInt(width.W))
    })
    io.zero  := !io.in.orR()
    val cases = (0 until width).map { i =>
      (io.in === BitPat(s"b${"0"*(width-1-i)}1${"?"*i}")) -> s"b${"0"*(width-1-i)}1${"0"*i}".U
    }
    io.out := MuxCase(0.U, cases)
  }

  /** Leading-one position detector
   * @param width the width of the leading-one detector
   */
  class LOPD(width: Int) extends Module {
    require(width >= 1, "width of leading-one position detector must be positive")
    val io = IO(new Bundle {
      val in   = Input(UInt(width.W))
      val zero = Output(Bool())
      val out  = Output(UInt(log2Up(width).W))
    })
    io.zero  := !io.in.orR()
    val cases = (0 until width).map { i =>
      (io.in === BitPat(s"b${"0"*(width-1-i)}1${"?"*i}")) -> i.U
    }
    io.out := MuxCase(0.U, cases)
  }
}
