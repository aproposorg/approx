package approx.addition

import chisel3._

/** Truncation-error-tolerant adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than or equal to the width)
 * 
 * Interpretation of the adder of Zhu et al. [2010]
 */
class ETAI(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

  /** Controllable XOR cell
   * 
   * @param a the first operand
   * @param b the second operand
   * @param ctrl the control signal of the module
   * @return a controllable XOR
   * 
   * Implements the same logic as in the paper, although not necessarily using as few transistors.
   */
  private[ETAI] def cXOR(a: Bool, b: Bool, ctrl: Bool) = Mux(!ctrl, a ^ b, true.B)

  /** Control signal generation cell type I
   * 
   * @param a the first operand
   * @param b the second operand
   * @param ctrl1 the control signal from the first position in the next stage
   * @return a combined control signal (type I)
   */
  private[ETAI] def csgcI(a: Bool, b: Bool, ctrl1: Bool) = (a & b) | ctrl1

  /** Control signal generation cell type II
   * 
   * @param a the first operand
   * @param b the second operand
   * @param ctrl1 the control signal from the first position in the next stage
   * @param ctrlt the control signal from the t'th position in the next stage
   * @return a combined control signal (type II)
   */
  private[ETAI] def csgcII(a: Bool, b: Bool, ctrl1: Bool, ctrlt: Bool) = (a & b) | ctrl1 | ctrlt

  // Control signal generation
  val ctrl = Wire(Vec(width, Bool()))
  val sCtrlIns = Wire(Vec(stages-1, Vec(2, Bool())))
  (0 until stages).foreach { s =>
    val intCtrlIns = Wire(Vec(stageWidth, Bool()))
    // Generate t-1 lowest control signals
    (0 until stageWidth-1).foreach { i =>
      intCtrlIns(i) := csgcI(io.a(s*stageWidth + i), io.b(s*stageWidth + i), intCtrlIns(i+1))
    }

    // Generate t'th control signal
    if (s == stages-1) {
      intCtrlIns(stageWidth-1) := csgcI(io.a(s*stageWidth + stageWidth-1), io.b(s*stageWidth + stageWidth-1), false.B)
    } else {
      intCtrlIns(stageWidth-1) := csgcII(io.a(s*stageWidth + stageWidth-1), io.b(s*stageWidth + stageWidth-1), sCtrlIns(s)(0), sCtrlIns(s)(1))
    }

    // Pass controls to previous stage
    if (s != 0) {
      sCtrlIns(s-1) := VecInit(intCtrlIns(0), intCtrlIns(stageWidth-1))
    }

    // Output controls
    (0 until stageWidth).foreach { i => ctrl(s*stageWidth + i) := intCtrlIns(i) }
  }

  // Approximate sum generation
  val sums = Wire(Vec(width, Bool()))
  (0 until width).foreach { i =>
    sums(i) := cXOR(io.a(i), io.b(i), ctrl(i))
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := false.B
}
