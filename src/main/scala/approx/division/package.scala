package approx

import chisel3._

package object division {
  /** Combinational divider IO bundle
   * 
   * @param width the width of the divider
   */
  class DividerIO(width: Int) extends Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val q = Output(UInt(width.W))
    val r = Output(UInt(width.W))
  }

  /** Abstract combinational divider module class
   * 
   * @param width the width of the divider
   */
  abstract class Divider(val width: Int) extends Module {
    val io = IO(new DividerIO(width))
  }

  /** Sequential divider IO bundle
   * 
   * @param width the width of the divider
   */
  class SeqDividerIO(width: Int) extends Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val q = Output(UInt(width.W))
    val r = Output(UInt(width.W))
    val start = Input(Bool())
    val busy  = Output(Bool())
    val done  = Output(Bool())
    val dbz   = Output(Bool())
  }

  /** Abstract sequential divider module class
   * 
   * @param width the width of the divider
   */
  abstract class SeqDivider(val width: Int) extends Module {
    val io = IO(new SeqDividerIO(width))
  }
}
