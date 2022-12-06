package approx

import chisel3._
import chisel3.util._

package object addition {
  /** Adder IO bundle
   * 
   * @param width the width of the adder
   */
  class AdderIO(width: Int) extends Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Input(Bool())
    val s = Output(UInt(width.W))
    val cout = Output(Bool())
  }

  /** Abstract adder module class
   * 
   * @param width the width of the adder
   */
  abstract class Adder(val width: Int) extends Module {
    val io = IO(new AdderIO(width))
  }

  /** Self-timed adder IO bundle
   * 
   * @param width the width of the adder
   */
  class SelfTimedAdderIO(width: Int) extends AdderIO(width) {
    val f = Output(Bool())
  }

  /** Abstract self-timed adder module class
   * 
   * @param width the width of the adder
   */
  abstract class SelfTimedAdder(val width: Int) extends Module {
    val io = IO(new SelfTimedAdderIO(width))
  }

  /** Half adder IO bundle */
  class HAIO extends Bundle {
    val x = Input(Bool())
    val y = Input(Bool())
    val s    = Output(Bool())
    val cout = Output(Bool())
  }

  /** Abstract half-adder module class */
  abstract class HA extends Module {
    val io = IO(new HAIO)
  }

  /** Full adder IO bundle */
  class FAIO extends Bundle {
    val x   = Input(Bool())
    val y   = Input(Bool())
    val cin = Input(Bool())
    val s    = Output(Bool())
    val cout = Output(Bool())
  }

  /** Abstract full-adder module class */
  abstract class FA extends Module {
    val io = IO(new FAIO)
  }
}
