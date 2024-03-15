package approx.addition

import chisel3._

/** SESA1 approximate full-adder
 * 
 * Implementation from Jha et al. [2023]
 */
class SESA1 extends FA {
  io.s := (io.x ^ io.y ^ io.cin) | (!io.x & !io.y & !io.cin)
  io.cout := ((io.x ^ io.y) & io.cin) | (io.x & io.y)
}

/** SESA2 approximate full-adder
 * 
 * Implementation from Jha et al. [2023]
 */
class SESA2 extends FA {
  io.s := false.B
  io.cout := ((io.x ^ io.y) & io.cin) | (io.x & io.y)
}

/** SESA3 approximate full-adder
 * 
 * Implementation from Jha et al. [2023]
 */
class SESA3 extends FA {
  io.s := true.B
  io.cout := ((io.x ^ io.y) & io.cin) | (io.x & io.y)
}
