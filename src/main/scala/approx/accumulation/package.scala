package approx

import chisel3._

import approx.multiplication.comptree.Signature

package object accumulation {
  
  /** @todo Extend all these with support for pipelining! */

  /** Accumulator IO bundle
   * 
   * @param accW the width of the accumulator
   */
  private[accumulation] abstract class AccumulatorIO(accW: Int) extends Bundle {
    val zero = Input(Bool())
    val acc  = Output(UInt(accW.W))
  }

  /** Simple accumulator IO bundle
   * 
   * @param inW the width of the input operand
   * @param accW the width of the accumulator
   */
  class SimpleAccumulatorIO(inW: Int, accW: Int) extends AccumulatorIO(accW) {
    val in = Input(UInt(inW.W))
  }

  /** Multiply accumulator IO bundle
   * 
   * @param inAW the width of the first input operand
   * @param inBW the width of the second input operand
   * @param accW the width of the accumulator
   */
  class MultiplyAccumulatorIO(inAW: Int, inBW: Int, accW: Int) extends AccumulatorIO(accW) {
    val a = Input(UInt(inAW.W))
    val b = Input(UInt(inBW.W))
  }

  /** Bit matrix accumulator IO bundle
   * 
   * @param sig the input bit matrix' signature
   * @param accW the width of the accumulator
   */
  class MatrixAccumulatorIO[T <: Signature](sig: T, accW: Int) extends AccumulatorIO(accW) {
    val in = Input(UInt(sig.count.W))
  }

  /** Parallel simple accumulator IO bundle
   * 
   * @param nIn the number of parallel input operands
   * @param inW the width of the input operands
   * @param accW the width of the accumulator
   */
  class ParallelSimpleAccumulatorIO(nIn: Int, inW: Int, accW: Int) extends AccumulatorIO(accW) {
    val ins = Input(Vec(nIn, UInt(inW.W)))
  }

  /** Parallel multiply accumulator IO bundle
   * 
   * @param nIn the number of parallel input operands
   * @param inAW the width of the first input operands
   * @param inBW the width of the second input operands
   * @param accW the width of the accumulator
   */
  class ParallelMultiplyAccumulatorIO(nIn: Int, inAW: Int, inBW: Int, accW: Int) extends AccumulatorIO(accW) {
    val as = Input(Vec(nIn, UInt(inAW.W)))
    val bs = Input(Vec(nIn, UInt(inBW.W)))
  }

  /** Abstract simple accumulator module class
   * 
   * @param inW the width of the input operand
   * @param accW the width of the accumulator
   * @param signed whether input operands are signed
   */
  abstract class SA(val inW: Int, val accW: Int, val signed: Boolean) extends Module {
    val io = IO(new SimpleAccumulatorIO(inW, accW))
  }

  /** Abstract multiply accumulator module class
   * 
   * @param inAW the width of the first input operand
   * @param inBW the width of the second input operand
   * @param accW the width of the accumulator
   * @param signed whether input operands are signed
   * 
   * @todo Extend with different signs.
   */
  abstract class MAC(val inAW: Int, val inBW: Int, val accW: Int, val signed: Boolean) extends Module {
    val io = IO(new MultiplyAccumulatorIO(inAW, inBW, accW))
  }

  /** Abstract bit matrix accumulator module class
   * 
   * @param sig the input bit matrix' signature
   * @param accW the width of the accumulator
   */
  abstract class MxAC(val sig: Signature, val accW: Int) extends Module {
    val io = IO(new MatrixAccumulatorIO(sig, accW))
  }

  /** Parallel simple accumulator module class
   * 
   * @param nIn the number of parallel input operands
   * @param inW the width of the input operands
   * @param accW the width of the accumulator
   * @param signed whether the input operands are signed
   */
  abstract class PSA(val nIn: Int, val inW: Int, val accW: Int, val signed: Boolean) extends Module {
    val io = IO(new ParallelSimpleAccumulatorIO(nIn, inW, accW))
  }

  /** Parallel multiply accumulator module class
   * 
   * @param nIn the number of parallel input operands
   * @param inAW the width of the first input operands
   * @param inBW the width of the second input operands
   * @param accW the width of the accumulator
   * @param signed whether the input operands are signed
   * 
   * @todo Extend with different signs.
   */
  abstract class PMAC(val nIn: Int, val inAW: Int, val inBW: Int, val accW: Int, val signed: Boolean) extends Module {
    val io = IO(new ParallelMultiplyAccumulatorIO(nIn, inAW, inBW, accW))
  }
}
