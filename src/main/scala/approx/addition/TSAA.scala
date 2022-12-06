package approx.addition

import chisel3._

/** TSAA approximate full-adder
 * 
 * Implementation from Yang and Thapliyal [2020]
 */
class TSAA extends FA {
  val sum = io.x ^ io.y ^ io.cin
  io.s := sum
  io.cout := !sum
}
