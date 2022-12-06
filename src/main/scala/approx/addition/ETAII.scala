package approx.addition

import chisel3._

/** Error-tolerant adder II
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than or equal to the width)
 * 
 * Implementation of the adder from Zhu et al. [2009]
 */
class ETAII(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

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
    val cg = Module(new CarryGen(stageWidth))
    cg.io.p   := pVec(i)
    cg.io.g   := gVec(i)
    cg.io.cin := false.B // from definition of ETAII
    carries(i) := cg.io.carries
    couts(i+1) := cg.io.cout
  }

  // Combine results and output
  io.s := pVec.asUInt ^ carries.asUInt
  io.cout := couts(stages)
}

/** Modified error-tolerant adder II
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than or equal to the width)
 * 
 * Implementation of the adder from Zhu et al. [2009]
 */
class ETAIIM(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

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
    val cg = Module(new CarryGen(stageWidth))
    cg.io.p   := pVec(i)
    cg.io.g   := gVec(i)
    if (i < stages/2) // from definition of ETAIIM
      cg.io.cin := false.B
    else
      cg.io.cin := couts(i)
    carries(i) := cg.io.carries
    couts(i+1) := cg.io.cout
  }

  // Combine results and output
  io.s := pVec.asUInt ^ carries.asUInt
  io.cout := couts(stages)
}
