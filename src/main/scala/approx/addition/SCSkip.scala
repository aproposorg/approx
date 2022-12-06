package approx.addition

import chisel3._

/** Speculative Carry Skip adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than or equal to the width)
 * 
 * Implementation of the adder from Kim et al. [2013]
 */
class SCSkip(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

  // Split operands
  val aVec = io.a.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val bVec = io.b.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val pVec = (io.a ^ io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val gVec = (io.a & io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))

  // Generate sum
  val cins  = Wire(Vec(stages, Bool()))
  cins(0)  := io.cin
  val sums  = Wire(Vec(stages, UInt(stageWidth.W)))
  val cgs   = Array.fill(stages) { Module(new CarryGen(stageWidth)) }
  (0 until stages).foreach { i =>
    cgs(i).io.p := pVec(i)
    cgs(i).io.g := gVec(i)

    // Pass carry-in to carry generator only in first stage
    if (i == 0)
      cgs(i).io.cin := io.cin
    else
      cgs(i).io.cin := false.B

    // Keep track of and skip carries
    if (i != 0)
      cins(i) := Mux(pVec(i).asUInt.andR, cgs(i-1).io.cout, cgs(i).io.cout)

    // Finally generate the sum
    sums(i) := pVec(i) ^ (cgs(i).io.carries(stageWidth-1, 1) ## cins(i))
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := cgs(stages-1).io.cout
}
