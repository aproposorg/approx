package approx

import chisel3._
import chisel3.util._

package object division {
  /** Divider IO bundle
   * 
   * @param width the width of the divider
   */
  class DividerIO(width: Int) extends Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val q = Output(UInt(width.W))
    val r = Output(UInt(width.W))
    val start = Input(Bool())
    val busy  = Output(Bool())
    val done  = Output(Bool())
    val dbz   = Output(Bool())
  }

  /** Abstract divider module class
   * 
   * @param conf a configuration case class
   */
  abstract class Divider(val width: Int) extends Module {
    val io = IO(new DividerIO(width))
  }
}
