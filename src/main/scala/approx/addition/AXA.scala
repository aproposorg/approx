package approx.addition

import chisel3._

/** AXA1 approximate full-adder
 * 
 * Implementation from Yang et al. [2013]
 */
class AXA1 extends FA {
  io.s := io.cin
  io.cout := ((io.x ^ io.y) & io.cin) | (!io.x & !io.y)
}

/** AXA2 approximate full-adder
 * 
 * Implementation from Yang et al. [2013]
 */
class AXA2 extends FA {
  io.s := !(io.x ^ io.y)
  io.cout := ((io.x ^ io.y) & io.cin) | (io.x & io.y)
}

/** AXA3 approximate full-adder
 * 
 * Implementation from Yang et al. [2013]
 */
class AXA3 extends FA {
  io.s := !(io.x ^ io.y) & io.cin
  io.cout := ((io.x ^ io.y) & io.cin) | (io.x & io.y)
}
