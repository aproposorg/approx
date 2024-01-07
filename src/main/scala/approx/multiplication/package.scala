package approx

import chisel3._
import chisel3.util.Decoupled

package object multiplication {

  /** 2x2-bit multiplier IO bundle */
  class TwoXTwoMultIO extends Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))
    val p = Output(UInt(4.W))
  }

  /** Abstract 2x2-bit multiplier module class */
  abstract class TwoXTwoMult extends Module {
    val io = IO(new TwoXTwoMultIO)
  }

  /** Multiplier IO bundle
   * 
   * @param aW the width of the first operand
   * @param bW the width of the second operand
   */
  class MultiplierIO(aW: Int, bW: Int) extends Bundle {
    val a = Input(UInt(aW.W))
    val b = Input(UInt(bW.W))
    val p = Output(UInt((aW+bW).W))
  }

  /** Abstract multiplier module class
   * 
   * @param aWidth the width of the first operand
   * @param bWidth the width of the second operand
   */
  abstract class Multiplier(val aWidth: Int, val bWidth: Int) extends Module {
    val io = IO(new MultiplierIO(aWidth, bWidth))
  }

  /** Sequential multiplier IO bundle
   * 
   * @param w the width of the multiplier
   */
  class SeqMultiplierIO(w: Int) extends Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val a = UInt(w.W)
      val b = UInt(w.W)
    }))
    val out = Decoupled(new Bundle {
      val p = UInt((2*w).W)
    })
  }

  /** Abstract sequential multiplier module class
   * 
   * @param width the width of the multiplier
   */
  abstract class SeqMultiplier(val width: Int) extends Module {
    val io = IO(new SeqMultiplierIO(width))
  }

  /** Compressor 2:2 IO bundle */
  class C2to2IO extends Bundle {
    val x1 = Input(Bool())
    val x2 = Input(Bool())
    val s    = Output(Bool())
    val cout = Output(Bool())
  }

  /** Abstract compressor 2:2 module class */
  abstract class C2to2 extends Module {
    val io = IO(new C2to2IO)
  }

  /** Compressor 3:2 IO bundle */
  class C3to2IO extends Bundle {
    val x1 = Input(Bool())
    val x2 = Input(Bool())
    val x3 = Input(Bool())
    val s    = Output(Bool())
    val cout = Output(Bool())
  }

  /** Abstract compressor 3:2 module class */
  abstract class C3to2 extends Module {
    val io = IO(new C3to2IO)
  }

  /** Compressor 4:2 IO bundle */
  class C4to2IO extends Bundle {
    val x1  = Input(Bool())
    val x2  = Input(Bool())
    val x3  = Input(Bool())
    val x4  = Input(Bool())
    val cin = Input(Bool())
    val s    = Output(Bool())
    val c    = Output(Bool())
    val cout = Output(Bool())
  }

  /** Abstract compressor 4:2 module class */
  abstract class C4to2 extends Module {
    val io = IO(new C4to2IO)
  }

  /** Compressor 5:3 IO bundle */
  class C5to3IO extends Bundle {
    val x1 = Input(Bool())
    val x2 = Input(Bool())
    val x3 = Input(Bool())
    val x4 = Input(Bool())
    val x5 = Input(Bool())
    val s  = Output(Bool())
    val c1 = Output(Bool())
    val c2 = Output(Bool())
  }

  /** Abstract compressor 5:3 module class */
  abstract class C5to3 extends Module {
    val io = IO(new C5to3IO)
  }

  /** Compressor 7:3 IO bundle */
  class C7to3IO extends Bundle {
    val x1 = Input(Bool())
    val x2 = Input(Bool())
    val x3 = Input(Bool())
    val x4 = Input(Bool())
    val x5 = Input(Bool())
    val x6 = Input(Bool())
    val x7 = Input(Bool())
    val s  = Output(Bool())
    val c1 = Output(Bool())
    val c2 = Output(Bool())
  }

  /** Abstract compressor 7:3 module class */
  abstract class C7to3 extends Module {
    val io = IO(new C7to3IO)
  }
}
