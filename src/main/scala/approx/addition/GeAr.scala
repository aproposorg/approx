package approx.addition

import chisel3._

/** Generic accuracy-configurable approximate adder IO bundle
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages
 * 
 * Ctrl inputs are used for enabling error correction, stall output indicates error.
 */
class GeArIO(width: Int, stages: Int) extends AdderIO(width) {
  val ctrl = Input(Vec(stages-1, Bool()))
  val stall = Output(Bool())
}

/** Generic accuracy-configurable approximate adder
 * 
 * @param width the width of the adder
 * @param subAddWidth the width of the sub-adders (must be less than the width)
 * @param specWidth the number of bits used for speculation (must be less than the width)
 * @param hasECC whether to implement error detection and correction (defaults to False)
 * 
 * Implementation of the adder from Shafique et al. [2015]
 * 
 * Does not, per default, implement the error detection and correction originally proposed in the paper. Users can set conf.ecc = true to enable this with the necessary register.
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class GeAr(val width: Int, val subAddWidth: Int, val specWidth: Int, val hasECC: Boolean = false) extends Module {
  val subWidth = subAddWidth + specWidth // total width of the sub-adders
  val stages   = ((width - subWidth) / subAddWidth) + 1
  require((subWidth + (stages-1)*subAddWidth) == width, "widths of sub-stages must add up to the total width")

  val io = IO(new GeArIO(width, stages))

  // Split operands
  val aVec = Wire(Vec(stages, UInt(subWidth.W)))
  val bVec = Wire(Vec(stages, UInt(subWidth.W)))
  (0 until stages).foreach { i =>
    val (top, bottom) = (i * subAddWidth + subWidth - 1, i * subAddWidth)
    aVec(i) := io.a(top, bottom)
    bVec(i) := io.b(top, bottom)
  }

  // Carry and sum generation
  val cins    = Wire(Vec(stages, Bool()))
  cins(0)    := io.cin
  (1 until stages).foreach { i => cins(i) := false.B }
  // ... carry prediction and carry out signals
  val cpreds  = Wire(Vec(stages, Bool()))
  val couts   = Wire(Vec(stages, Bool()))
  // ... intermediate sums
  val sums    = Wire(Vec(stages, UInt(subWidth.W)))
  // ... error correction stuff
  val errNext = Wire(Vec(stages-1, Bool()))
  val err     = RegNext(errNext)
  val correct = Wire(Vec(stages-1, Bool()))
  (0 until stages-1).foreach { i =>
    errNext(i) := couts(i) & cpreds(i+1)
    correct(i) := io.ctrl(i) & err(i)
  }

  // Do the generation here
  (0 until stages).foreach { i =>
    val cg = Module(new CarryGen(subWidth))
    val p  = Wire(UInt(subWidth.W))
    val g  = Wire(UInt(subWidth.W))
    if (!hasECC || i == 0) {
      p := aVec(i) ^ bVec(i)
      g := aVec(i) & bVec(i)
    } else {
      val repl = ((BigInt(1) << subWidth) - 1).U
      p := Mux(correct(i-1), repl ^ repl, aVec(i) ^ bVec(i))
      g := Mux(correct(i-1), repl & repl, aVec(i) & bVec(i))
    }
    cg.io.p   := p
    cg.io.g   := g
    cg.io.cin := cins(i)
    cpreds(i) := cg.io.carries(specWidth-1)
    couts(i)  := cg.io.cout
    sums(i)   := cg.io.carries.asUInt ^ p
  }

  // Combine results and output
  io.s     := (1 until stages).foldLeft(sums(0)) { (acc, i) => sums(i)(subWidth-1, subWidth-subAddWidth) ## acc }
  io.cout  := couts(stages-1)
  if (!hasECC) {
    io.stall := false.B
  } else {
    // If k == 1, no error control ...
    if (stages == 1)
      io.stall := false.B
    // ... otherwise, implement the error control
    else
      io.stall := correct.asUInt.orR
  }
}
