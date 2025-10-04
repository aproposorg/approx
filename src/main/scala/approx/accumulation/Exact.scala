package approx.accumulation

import chisel3._
import chisel3.util.RegEnable
import chisel3.util.experimental.FlattenInstance

import approx.util.PRShiftReg
import approx.multiplication.comptree.{Approximation, Signature, CompressorTree}

/** Simple accumulator
 * 
 * @param inW the width of the input operand
 * @param accW the width of the accumulator
 * @param signed whether the input operands are signed (defaults to false)
 * @param pipes the number of pipeline stages (defaults to 0)
 * 
 * Pipelining relies on retiming!
 */
class SimpleAccumulator(inW: Int, accW: Int, signed: Boolean = false, pipes: Int = 0)
  extends SA(inW, accW, signed, pipes) {
  // Extend the input to the width of the accumulator if needed
  val inExt = if (inW < accW) {
    val sext = if (signed) VecInit(Seq.fill(accW - inW)(io.in(inW-1))).asUInt else 0.U((accW - inW).W)
    sext ## io.in
  } else io.in(accW-1, 0)

  // Pass the extended input through a series of registers
  val dataShReg = Module(new PRShiftReg(UInt(accW.W), pipes))
  dataShReg.io.in := inExt

  // Pass enable and zero through a shift register as needed
  val enShReg = Module(new PRShiftReg(Bool(), pipes))
  enShReg.io.in := io.en
  val zeroShReg = Module(new PRShiftReg(Bool(), pipes))
  zeroShReg.io.in := io.zero

  // Compute the sum and register the accumulator
  val sum = Wire(UInt(accW.W))
  val acc = RegEnable(sum, 0.U(accW.W), enShReg.io.out.last)
  sum    := dataShReg.io.out.last + Mux(zeroShReg.io.out.last, 0.U, acc)
  io.acc := acc
}

/** Multiply accumulator
 * 
 * @param inAW the width of the first input operand
 * @param inBW the width of the second input operand
 * @param accW the width of the accumulator
 * @param signed whether the input operands are signed (defaults to false)
 * @param pipes the number of pipeline stages (defaults to 0)
 * 
 * Pipelining relies on retiming!
 */
class MultiplyAccumulator(inAW: Int, inBW: Int, accW: Int, signed: Boolean = false, pipes: Int = 0)
  extends MAC(inAW, inBW, accW, signed, pipes) {
  // Compute and extend the product to the width of the accumulator if needed
  val prodExt = if ((inAW + inBW) < accW) {
    val prod = if (signed) (io.a.asSInt * io.b.asSInt).asUInt else io.a * io.b
    val sext = if (signed) VecInit(Seq.fill(accW - inAW - inBW)(prod(inAW + inBW - 1))).asUInt else 0.U((accW - inAW - inBW).W)
    sext ## prod
  } else {
    (if (signed) (io.a.asSInt * io.b.asSInt) else (io.a * io.b))(accW-1, 0)
  }

  // Pass the extended product through a series of registers
  val dataShReg = Module(new PRShiftReg(UInt(accW.W), pipes))
  dataShReg.io.in := prodExt

  // Pass enable and zero through a shift register as needed
  val enShReg = Module(new PRShiftReg(Bool(), pipes))
  enShReg.io.in := io.en
  val zeroShReg = Module(new PRShiftReg(Bool(), pipes))
  zeroShReg.io.in := io.zero

  // Compute the sum and register the accumulator
  val sum = Wire(UInt(accW.W))
  val acc = RegEnable(sum, 0.U(accW.W), enShReg.io.out.last)
  sum := dataShReg.io.out.last + Mux(zeroShReg.io.out.last, 0.U, acc)
  io.acc := acc
}

/** Bit matrix accumulator
 * 
 * @param sig the input bit matrix' signature
 * @param accW the width of the accumulator
 * @param pipes the number of pipeline stages (defaults to 0)
 * @param targetDevice a string indicating the target device
 *                     (defaults to "", meaning ASIC)
 * @param mtrc which metric to use for selecting counters (defaults to efficiency)
 * @param approx the targeted approximation styles (defaults to no approximation)
 * 
 * Pipelining relies on retiming!
 * 
 * @todo Consider building pipelining into the compressor tree generator.
 */
class BitMatrixAccumulator(sig: Signature, accW: Int, pipes: Int = 0, targetDevice: String = "",
  mtrc: Char = 'e', approx: Seq[Approximation] = Seq.empty[Approximation])
  extends MxAC(sig, accW, pipes) with FlattenInstance {
  // Add the accumulator to the input signature
  val sigExt = new Signature((0 until scala.math.max(accW, sig.length)).map { c =>
    val sigCnt = if (c < sig.length) sig.signature(c) else 0
    val accCnt = if (c < accW) 1 else 0
    sigCnt + accCnt
  }.toArray)

  // Build a compressor tree and assign its inputs and outputs
  val acc     = Wire(UInt(accW.W))
  val comp    = Module(CompressorTree(sigExt, targetDevice=targetDevice, mtrc=mtrc, approx=approx))
  val compIns = Wire(Vec(sigExt.count, Bool()))
  var (inOffset, compOffset) = (0, 0)
  (0 until scala.math.max(accW, sig.length)).foreach { c =>
    // Add the signature's input bits
    if (c < sig.length) {
      (0 until sig.signature(c)).foreach { _ =>
        compIns(compOffset) := io.in(inOffset)
        compOffset += 1
        inOffset += 1
      }
    }

    // Add the accumulator bit
    if (c < accW) {
      compIns(compOffset) := !io.zero & acc(c)
      compOffset += 1
    }
  }

  // Pass the compressor output through a series of registers
  val dataShReg = Module(new PRShiftReg(UInt(accW.W), pipes))
  comp.io.in := compIns.asUInt
  dataShReg.io.in := comp.io.out

  // Pass enable through a shift register as needed
  val enShReg = Module(new PRShiftReg(Bool(), pipes))
  enShReg.io.in := io.en

  // Compute the sum and register the accumulator
  val accReg = RegEnable(comp.io.out, 0.U(accW.W), enShReg.io.out.last)
  acc    := accReg
  io.acc := accReg
}

/** Parallel simple accumulator
 * 
 * @param nIn the number of parallel input operands
 * @param inW the width of the input operands
 * @param accW the width of the accumulator
 * @param signed whether the input operands are signed (defaults to false)
 * @param pipes the number of pipeline stages (defaults to 0)
 * @param comp whether to use the compressor tree generator (defaults to false)
 * @param targetDevice a string indicating the target device
 *                     (defaults to "", meaning ASIC)
 * @param mtrc which metric to use for selecting counters (defaults to efficiency)
 * @param approx the targeted approximation styles (defaults to no approximation)
 * 
 * Pipelining relies on retiming!
 */
class ParallelSimpleAccumulator(nIn: Int, inW: Int, accW: Int, signed: Boolean = false,
  pipes: Int = 0, comp: Boolean = false, targetDevice: String = "", mtrc: Char = 'e',
  approx: Seq[Approximation] = Seq.empty[Approximation])
  extends PSA(nIn, inW, accW, signed, pipes) with FlattenInstance {
  // Extend the inputs to the width of the accumulator if needed
  val insExt = if (inW < accW) {
    if (signed) {
      VecInit(io.ins.map { in => VecInit(Seq.fill(accW - inW)(in(inW-1))).asUInt ## in })
    } else io.ins
  } else VecInit(io.ins.map(_(accW-1, 0)))

  // Depending on the parameters passed, generate a naive accumulator or use 
  // the custom compressor tree generator
  if (comp) {
    // Generate the signature of the needed compressor tree
    val extW = insExt.head.getWidth
    val sig  = new Signature(Array.fill(extW)(nIn))

    // Build a bit matrix accumulator and assign its inputs and outputs
    val mxAcc  = Module(new BitMatrixAccumulator(sig, accW, pipes, targetDevice, mtrc, approx))
    val accIns = VecInit((0 until extW).flatMap { c => (0 until nIn).map(i => insExt(i)(c)) }).asUInt

    mxAcc.io.en   := io.en
    mxAcc.io.zero := io.zero
    mxAcc.io.in   := accIns
    io.acc := mxAcc.io.acc
  } else {
    // Pass the parallel sum through a series of registers
    val dataShReg = Module(new PRShiftReg(UInt(accW.W), pipes))
    dataShReg.io.in := insExt.reduceTree(_ +& _)

    // Pass enable and zero through a shift register as needed
    val enShReg = Module(new PRShiftReg(Bool(), pipes))
    enShReg.io.in := io.en
    val zeroShReg = Module(new PRShiftReg(Bool(), pipes))
    zeroShReg.io.in := io.zero

    // Compute the sum and register the accumulator
    val sum = Wire(UInt(accW.W))
    val acc = RegEnable(sum, 0.U(accW.W), enShReg.io.out.last)
    sum    := dataShReg.io.out.last + Mux(zeroShReg.io.out.last, 0.U, acc)
    io.acc := acc
  }
}

/** Parallel multiply accumulator
 * 
 * @param nIn the number of parallel input operands
 * @param inAW the width of the first input operands
 * @param inBW the width of the second input operands
 * @param accW the width of the accumulator
 * @param signed whether the input operands are signed (defaults to false)
 * @param pipes the number of pipeline stages (defaults to 0)
 * @param comp whether to use the compressor tree generator (defaults to false)
 * @param targetDevice a string indicating the target device
 *                     (defaults to "", meaning ASIC)
 * @param mtrc which metric to use for selecting counters (defaults to efficiency)
 * @param approx the targeted approximation styles (defaults to no approximation)
 * 
 * Pipelining relies on retiming!
 */
class ParallelMultiplyAccumulator(nIn: Int, inAW: Int, inBW: Int, accW: Int, signed: Boolean = false,
  pipes: Int = 0, comp: Boolean = false, targetDevice: String = "", mtrc: Char = 'e',
  approx: Seq[Approximation] = Seq.empty[Approximation])
  extends PMAC(nIn, inAW, inBW, accW, signed, pipes) with FlattenInstance {

  // Depending on the parameters passed, generate a naive accumulator or use 
  // the custom compressor tree generator
  if (comp) {
    // Compute some constants and generate the sign-extension constant
    val midLo = scala.math.min(inAW, inBW) - 1
    val midHi = scala.math.max(inAW, inBW) - 1
    val upper = inAW + inBW - 1
    val extConst = if (signed) Seq.fill(nIn) {
      (BigInt(-1) << upper) + (BigInt(1) << midLo) + (BigInt(1) << midHi)
    }.sum else BigInt(0)

    /** Compute the number of dots in a column of a radix-2 partial product tree
     * 
     * @param col the index of the column
     * @return the number of bits in the column
     */
    def dotCount(col: Int): Int = {
      if (col < midLo) col + 1
      else if (midLo <= col && col <= midHi) scala.math.min(inAW, inBW)
      else if (col < upper) upper - col
      else 0
    }

    /** Compute the least significant row index to compress from within a given 
     * column of radix-2 partial products
     * 
     * @param col the index of the column
     * @return the index of the least significant row
     */
    def lsRow(col: Int): Int = if (col < inBW) 0 else (col - inBW + 1)

    // Generate the signature of the needed compressor tree
    val sig = new Signature((0 until scala.math.max(upper + 1, accW)).map { c =>
      val prodContr = nIn * dotCount(c)
      val extContr  = if (extConst.testBit(c)) 1 else 0
      prodContr + extContr
    }.toArray)

    // Compute the partial products
    val prods = if (signed) {
      (0 until nIn).map { i =>
        (0 until inAW).map { r =>
          val pprod = VecInit((0 until inBW).map { c =>
            val dot = io.as(i)(r) & io.bs(i)(c)
            if (c == (inBW - 1)) !dot else dot
          }).asUInt
          if (r == (inAW - 1)) ~pprod else pprod
        }
      }
    } else {
      (0 until nIn).map { i =>
        (0 until inAW).map { r => VecInit(Seq.fill(inBW)(io.as(i)(r))).asUInt & io.bs(i) }
      }
    }

    // Build a bit matrix accumulator and assign its inputs and outputs
    val mxAcc  = Module(new BitMatrixAccumulator(sig, accW, pipes, targetDevice, mtrc, approx))
    val accIns = Wire(Vec(sig.count, Bool()))
    var compOffset = 0
    (0 until sig.length).foreach { c =>
      // Add the partial product bits
      val low  = lsRow(c)
      val high = low + dotCount(c)
      (0 until nIn).foreach { i =>
        (low until high).foreach { r =>
          accIns(compOffset) := prods(i)(r)(c-r)
          compOffset += 1
        }
      }

      // Add the sign-extension bit
      if (extConst.testBit(c)) {
        accIns(compOffset) := true.B
        compOffset += 1
      }
    }

    mxAcc.io.en   := io.en
    mxAcc.io.zero := io.zero
    mxAcc.io.in   := accIns.asUInt
    io.acc := mxAcc.io.acc
  } else {
    // Compute and sign-extend the incoming products as needed
    val prodsExt = if ((inAW + inBW) < accW) {
      VecInit(io.as.zip(io.bs).map { case (a, b) =>
        val prod = if (signed) (a.asSInt * b.asSInt).asUInt else (a * b)
        val sext = if (signed) VecInit(Seq.fill(accW - inAW - inBW)(prod(inAW - inBW - 1))).asUInt else 0.U((accW - inAW - inBW).W)
        sext ## prod
      })
    } else {
      VecInit(io.as.zip(io.bs).map { case (a, b) =>
        (if (signed) (a.asSInt * b.asSInt) else (a * b))(accW-1, 0)
      })
    }

    // Pass the parallel sum through a series of registers
    val dataShReg = Module(new PRShiftReg(UInt(accW.W), pipes))
    dataShReg.io.in := prodsExt.reduceTree(_ +& _)

    // Pass enable and zero through a shift register as needed
    val enShReg = Module(new PRShiftReg(Bool(), pipes))
    enShReg.io.in := io.en
    val zeroShReg = Module(new PRShiftReg(Bool(), pipes))
    zeroShReg.io.in := io.zero

    // Connect and sum the extended products
    val sum = Wire(UInt(accW.W))
    val acc = RegEnable(sum, 0.U(accW.W), enShReg.io.out.last)
    sum    := dataShReg.io.out.last + Mux(zeroShReg.io.out.last, 0.U, acc)
    io.acc := acc
  }
}
