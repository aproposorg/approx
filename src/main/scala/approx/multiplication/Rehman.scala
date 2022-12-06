package approx.multiplication

import chisel3._

/** Approximate 2x2-bit multiplier
 * 
 * Implementation of the 2nd multiplier from Rehman et al. [2016]
 */
class ApproxMul2 extends TwoXTwoMult {
  val a0b1 = io.a(0) & io.b(1)
  val a1b0 = io.a(1) & io.b(0)
  val p0   = a0b1 & a1b0
  io.p    := p0 ## (p0 ^ (io.a(1) & io.b(1))) ## (a0b1 ^ a1b0) ## p0
}

/** Approximate 2x2-bit multiplier
 * 
 * Implementation of the 3rd multiplier from Rehman et al. [2016]
 */
class ApproxMul3 extends TwoXTwoMult {
  val all  = io.a.andR & io.b.andR
  io.p    := false.B ## ((io.a(1) & io.b(1)) ^ all) ## ((io.a(0) & io.b(1)) ^ (io.a(1) & io.b(0))) ## (io.a(0) & io.b(0))
}

/** Approximate 2x2-bit multiplier
 * 
 * Implementation of the 4th multiplier from Rehman et al. [2016]
 */
class ApproxMul4 extends TwoXTwoMult {
  io.p := false.B ## (io.a(1) & io.b(1)) ## ((io.a(0) & io.b(1)) ^ (io.a(1) & io.b(0))) ## (io.a(0) & io.b(0))
}

/** Approximate 2x2-bit multiplier
 * 
 * Implementation of the 5th multiplier from Rehman et al. [2016]
 */
class ApproxMul5 extends TwoXTwoMult {
  val p0 = io.a(0) & io.b(0)
  io.p := false.B ## !(p0 | (io.a(1) & io.b(1))) ## ((io.a(0) & io.b(1)) ^ (io.a(1) & io.b(0))) ## p0
}
