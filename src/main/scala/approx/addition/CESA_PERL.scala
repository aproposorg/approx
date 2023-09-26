package approx.addition

import chisel3._

/** Carry Estimating Simultaneous Adder with Propagating Error Rectification Logic
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than the width)
 * 
 * Implementation of the adder from Bhattacharjya et al. [2020]
 */
class CESA_PERL(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 4, "width of stages must be at least 4")
  require(stages < width, "number of stages must be less than the width")

  /** CESA adder stage
   * 
   * @param w the width of the adder stage
   */
  private[CESA_PERL] class CESA(w: Int) extends Module {
    val io = IO(new AdderIO(w))

    /** Compute the CEU function */
    val ceu = (io.a(w-1) & io.b(w-1)) | (io.a(w-2) & io.b(w-2) & (io.a(w-1) | io.b(w-1)))

    /** Compute the PERL function */
    val perl = (io.a(w-3) & io.b(w-3)) | (io.a(w-4) & io.b(w-4) & (io.a(w-3) | io.b(w-3)))

    /** Compute the selection unit logic */
    val sel = (io.a(w-1) ^ io.b(w-1)) & (io.a(w-2) ^ io.b(w-2))

    // Combine all the functions into the stage
    io.s    := io.a + io.b + io.cin
    io.cout := Mux(sel, perl, ceu)
  }

  // Split operands
  val aVec = io.a.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val bVec = io.b.asTypeOf(Vec(stages, UInt(stageWidth.W)))

  // Sum and carry generation
  val sums  = Wire(Vec(stages, UInt(stageWidth.W)))
  val couts = Wire(Vec(stages+1, Bool()))
  couts(0) := io.cin
  (0 until stages).foreach { i =>
    val cesa = Module(new CESA(stageWidth))
    cesa.io.a   := aVec(i)
    cesa.io.b   := bVec(i)
    cesa.io.cin := couts(i)
    sums(i)     := cesa.io.s
    couts(i+1)  := cesa.io.cout
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := couts(stages)
}
