package approx.multiplication

import chisel3._

/** Approximate 2x2-bit multiplier 
 * 
 * Implementation of the multiplier from Kulkarni et al. [2011]
 */
class Kulkarni extends TwoXTwoMult {
  io.p := false.B ## (io.a(1) & io.b(1)) ## ((io.a(0) & io.b(1)) | (io.a(1) & io.b(0))) ## (io.a(0) & io.b(0))
}
