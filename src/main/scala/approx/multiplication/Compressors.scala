package approx.multiplication

import chisel3._

/** 1st approximate compressor 4:2
 * 
 * Implementation of design 1 from Momeni et al. [2014]
 */
class Compressor4to2D1 extends C4to2 {
  io.s    := !io.cin & (!(io.x1 ^ io.x2) | !(io.x3 ^ io.x4))
  io.c    := io.cin
  io.cout := !(!(io.x1 & io.x2) | !(io.x3 & io.x4))
}

/** 2nd approximate compressor 4:2
 * 
 * Implementation of design 2 from Momeni et al. [2014<]
 */
class Compressor4to2D2 extends C4to2 {
  io.s    := !(io.x1 ^ io.x2) | !(io.x3 ^ io.x4)
  io.c    := !(!(io.x1 & io.x2) | !(io.x3 & io.x4))
  io.cout := false.B
}

/** Approximate compressor 4:2
 * 
 * Implementation of the compressor from Moaiyeri et al. [2017]
 */
class Compressor4to2Maj extends C4to2 {
  /** Maj function
   * 
   * @param a the first operand
   * @param b the second operand
   * @param c the third operand
   * @return true if a majority of the three inputs are asserted
   */
  def maj(a: Bool, b: Bool, c: Bool) = (a & (b | c)) | (b & c)
  io.s    := maj(io.x1, io.x2, !maj(io.x3, io.x4, !io.cin))
  io.c    := io.x4
  io.cout := io.x3
}
