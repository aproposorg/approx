package approx.addition

import chisel3._

/** Reconfigurable APproximate Carry Look-ahead adder IO bundle
 * 
 * @param width the width of the adder
 */
class RAP_CLAIO(width: Int) extends AdderIO(width) {
  val appx = Input(Bool())
}

/** Reconfigurable APproximate Carry Look-ahead adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than the width)
 * 
 * Implementation of the adder from Akbari et al. [2018]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class RAP_CLA(val width: Int, val stages: Int) extends Module {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

  val io = IO(new RAP_CLAIO(width))

  /** Approximate carry look-ahead-style carry generation
   * 
   * @param w the width of the generator
   */
  private[RAP_CLA] class AppxCarryGen(w: Int) extends Module {
    val io = IO(new Bundle {
      val p = Input(UInt(w.W))
      val g = Input(UInt(w.W))
      val cin = Input(Bool())
      val carries = Output(UInt(w.W))
      val cout = Output(Bool())

      val appx = Input(Bool())
    })

    // Exact carries
    val cs = Wire(Vec(w+1, Bool()))
    cs(0) := io.cin
    (0 until w).foreach { i => cs(i+1) := io.g(i) | (io.p(i) && cs(i)) }

    // Approximate carries (same as exact, except carry-in is constant low)
    val appxcs = Wire(Vec(w+1, Bool()))
    appxcs(0) := false.B
    (0 until w).foreach { i => appxcs(i+1) := io.g(i) | (io.p(i) && appxcs(i)) }

    // Output result
    io.carries := cs.asUInt(w-1, 0)
    io.cout    := Mux(io.appx, appxcs(w), cs(w))
  }

  // Split operands
  val aVec = io.a.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val bVec = io.b.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val pVec = (io.a ^ io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val gVec = (io.a & io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))

  // Carry generation
  val carries = Wire(Vec(stages, UInt(stageWidth.W)))
  val couts = Wire(Vec(stages+1, Bool()))
  couts(0) := io.cin
  (0 until stages).foreach { i =>
    val cg = Module(new AppxCarryGen(stageWidth))
    cg.io.p    := pVec(i)
    cg.io.g    := gVec(i)
    cg.io.cin  := couts(i)
    cg.io.appx := io.appx
    carries(i) := cg.io.carries
    couts(i+1) := cg.io.cout
  }

  // Combine results and output
  io.s    := pVec.asUInt ^ carries.asUInt
  io.cout := couts(stages)
}
