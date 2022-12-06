package approx.addition

import chisel3._

/** Self-adaptive approximate adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than the width)
 * 
 * Implementation of the adder from Liu et al. [2020]
 */
class SelfAdaptive(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

  val aVec = Wire(Vec(stages, UInt(stageWidth.W)))
  aVec := io.a.asTypeOf(aVec)
  val bVec = Wire(Vec(stages, UInt(stageWidth.W)))
  bVec := io.b.asTypeOf(bVec)
  val sums = Wire(Vec(stages, UInt(stageWidth.W)))
  val cins = Wire(Vec(stages, Bool()))
  cins(0) := io.cin
  val enables = Wire(Vec(stages-1, Bool()))

  // Generate the stages
  (0 until stages-1).foreach { s =>
    val sum = Wire(Vec(stageWidth, Bool()))
    val couts = Wire(Vec(stageWidth, Bool()))
    (0 until stageWidth).foreach { i =>
      val adder = Module(new FullAdder)
      adder.io.x := aVec(s)(i)
      adder.io.y := bVec(s)(i)
      if (i == 0)
        adder.io.cin := cins(s)
      else
        adder.io.cin := couts(i-1)
      sum(i)   := adder.io.s
      couts(i) := adder.io.cout
    }
    sums(s) := Mux(enables(s), aVec(s) | bVec(s), sum.asUInt)
    cins(s+1) := couts(stageWidth-1)
    if (s > 0)
      enables(s-1) := sum.asUInt.orR
  }
  val slast = Wire(Vec(stageWidth, Bool()))
  val clast = Wire(Vec(stageWidth, Bool()))
  (0 until stageWidth).foreach { i =>
    val adder = Module(new FullAdder)
    adder.io.x := aVec(stages-1)(i)
    adder.io.y := bVec(stages-1)(i)
    if (i == 0)
      adder.io.cin := cins(stages-1)
    else
      adder.io.cin := clast(i-1)
    slast(i) := adder.io.s
    clast(i) := adder.io.cout
  }
  sums(stages-1)    := slast.asUInt
  enables(stages-2) := slast.asUInt.orR

  // Combine results and output
  io.s := sums.asUInt
  io.cout := clast(stageWidth-1)
}
