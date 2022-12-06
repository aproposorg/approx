package approx.addition

import chisel3._

/** TCAA approximate full-adder
 * 
 * Implementation from Yang and Thapliyal [2020]
 */
class TCAA extends FA {
  val carry = (io.x & io.y) | (io.x & io.cin) | (io.y & io.cin)
  io.s := !carry
  io.cout := carry
}
