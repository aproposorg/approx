package approx.addition

import chisel3._

/** InXA1 approximate full-adder
 * 
 * Implementation from Almurib et al. [2016]
 */
class InXA1 extends FA {
  io.s := io.x ^ io.y ^ io.cin
  io.cout := io.cin
}

/** InXA1 approximate full-adder
 * 
 * Implementation from Almurib et al. [2016]
 */
class InXA2 extends FA {
  val p = io.x ^ io.y
  val g = io.x & io.y
  io.s := p | io.cin
  io.cout := g | (p & io.cin)
}

/** InXA1 approximate full-adder
 * 
 * Implementation from Almurib et al. [2016]
 */
class InXA3 extends FA {
  val p = io.x ^ io.y
  val g = io.x & io.y
  val res = g | (p & io.cin)
  io.s := !res
  io.cout := res
}
