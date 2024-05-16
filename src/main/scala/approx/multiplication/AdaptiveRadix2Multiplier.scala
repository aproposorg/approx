package approx.multiplication

import chisel3._
import chisel3.util.log2Up

import approx.multiplication.comptree._

/** Adaptive radix 2 combinational multiplier IO bundle
 * 
 * @param aWidth the width of the first operand
 * @param bWidth the width of the second operand
 * @param numModes the number of approximation modes
 */
class AdaptiveMultiplierIO(aW: Int, bW: Int, numModes: Int) extends MultiplierIO(aW, bW) {
  val ctrl = Input(UInt(log2Up(numModes+1).W))
}

/** Adaptive radix 2 combinational multiplier with error correction
 * 
 * @param aWidth the width of the first operand
 * @param bWidth the width of the second operand
 * @param approxWidth the width of the approximate part
 *                    (equal to the number of columns to approximate)
 * @param aSigned whether the first operand is signed (defaults to false)
 * @param bSigned whether the second operand is signed (defaults to false)
 * @param comp whether to use the compressor generator (defaults to false)
 * @param targetDevice a string indicating the target device (defaults to "",
 *                     meaning ASIC)
 * @param numModes the number of approximation modes (defaults to 1)
 * 
 * Adaptation of the multiplier from Frustaci et al. [2020]
 * 
 * Optionally makes use of the compressor tree generator to add partial products.
 * 
 * Does not inherit from [[Multiplier]] because of IO mismatch.
 */
class AdaptiveRadix2Multiplier(val aWidth: Int, val bWidth: Int, val approxWidth: Int,
  val aSigned: Boolean = false, val bSigned: Boolean = false, comp: Boolean = false,
  targetDevice: String = "", val numModes: Int = 1)
  extends Module with HasRadix2PartialProducts {
  require(approxWidth <= (aWidth + bWidth),
    "width of the approximate part must be less than or equal to the product width")
  require(numModes >= 1, "number of approximation modes must be positive")
  require((approxWidth / numModes) >= 1,
    "width of the approximate part must be divisible by the number of approximation modes")

  val io = IO(new AdaptiveMultiplierIO(aWidth, bWidth, numModes))

  // Determine the number of columns to approximate in each mode; 0 meaning 
  // fully exact multiplication
  val modeDists = Seq(0) ++ (1 until numModes).map(m => approxWidth / numModes * m) ++ Seq(approxWidth)

  // Compute the transmission vector and pick the proper bits for the partial 
  // product generation in the currently selected mode
  val trns    = WireDefault(VecInit(Seq.fill(approxWidth)(true.B)))
  val nPPTrns = scala.math.min(4, approxWidth / numModes)
  val ppTrns  = WireDefault(VecInit(Seq.fill(nPPTrns)(true.B)))
  modeDists.zipWithIndex.drop(1).foreach { case (mAWidth, mInd) =>
    when(io.ctrl === mInd.U) {
      // Set the bits of the transmission vector low
      (0 until mAWidth).foreach(i => trns(i) := false.B)

      // Pick the most significant subset of the bits
      (0 until nPPTrns).foreach(i => ppTrns(i) := trns(mAWidth - nPPTrns + i))
    }
  }

  // Depending on aSigned and bSigned, generate an unsigned or signed multiplier
  if (aSigned || bSigned) {
    // ... at least one operand is signed
    // Extend the unsigned operand by one bit
    val (aW, opA) = if (aSigned) (aWidth, io.a) else (aWidth+1, false.B ## io.a)
    val (bW, opB) = if (bSigned) (bWidth, io.b) else (bWidth+1, false.B ## io.b)
    val  pW = aW + bW

    // Create all the partial products
    val pprods = (0 until aW-1).map { i =>
      ~(opB(bW-1) & opA(i)) ## VecInit(opB.asBools.dropRight(1).map(_ & opA(i))).asUInt
    } :+ ((opB(bW-1) & opA(aW-1)) ## ~VecInit(opB.asBools.dropRight(1).map(_ & opA(aW-1))).asUInt)

    // Compute the positions of the sign-extension constants
    val midLow  = scala.math.min(aW, bW) - 1
    val midHigh = scala.math.max(aW, bW) - 1
    val upper   = aW + bW - 1

    // Create all the shifted partial products
    val shftPprods = WireDefault(VecInit(pprods.zipWithIndex.map { case (pprod, ind) =>
      VecInit((0.U((pW-bW-ind).W) ## (if (ind == 0) pprod else (pprod ## 0.U(ind.W)))).asBools) }))

    // Check whether the first and last partial product bits in a column 
    // are inverted (and therefore require alternative masking)
    def firstLastInv(col: Int): Boolean = (bW - 1) <= col && col < (pW - 2)

    // Apply the transmission bits to the shifted partial products
    val mskdPprods = WireDefault(shftPprods)
    modeDists.zipWithIndex.drop(1).foreach { case (mAWidth, mInd) =>
      when(io.ctrl === mInd.U) {
        // Apply the least significant transmission bit to some least 
        // significant columns
        (0 until mAWidth - nPPTrns).foreach { c =>
          val low  = lsCol(c, bW)
          val high = low + dotCount(c, aW, bW)
          if (firstLastInv(c)) {
            mskdPprods(low)(c) := !(shftPprods(low)(c) & ppTrns(0))
            (low + 1 until high - 1).foreach { r =>
              mskdPprods(r)(c) := shftPprods(r)(c) & ppTrns(0) }
            mskdPprods(high-1)(c) := !(shftPprods(high-1)(c) & ppTrns(0))
          } else {
            (low until high).foreach { r =>
              mskdPprods(r)(c) := shftPprods(r)(c) & ppTrns(0) }
          }
        }

        // Introduce some error correction in the more significant columns
        (mAWidth - nPPTrns until mAWidth).zipWithIndex.foreach { case (c, ind) =>
          val low  = lsCol(c, bW)
          val high = low + dotCount(c, aW, bW)
          if (firstLastInv(c)) {
            mskdPprods(low)(c) := !(shftPprods(low)(c) & ppTrns(ind))
            (low + 1 until high - 2).foreach { r =>
              mskdPprods(r)(c) := shftPprods(r)(c) & ppTrns(ind) }
            (high - 2 until high - 1).foreach { r =>
              mskdPprods(r)(c) := shftPprods(r)(c) | !ppTrns(ind) }
            mskdPprods(high-1)(c) := !(shftPprods(high-1)(c) & ppTrns(ind))
          } else {
            (low until high - 2).foreach { r =>
              mskdPprods(r)(c) := shftPprods(r)(c) &  ppTrns(ind) }
            (scala.math.max(0, high - 2) until high).foreach { r =>
              mskdPprods(r)(c) := shftPprods(r)(c) | !ppTrns(ind) }
          }
        }
      }
    }

    // Compute some error compensation constants
    val corrConsts = Seq(0.U(pW.W)) ++ modeDists.drop(1).map { mAWidth =>
      val errLow = (0 until mAWidth - nPPTrns).foldLeft(BigDecimal(0.0)) { case (acc, c) =>
        acc + 0.25 * dotCount(c, aW, bW) * BigDecimal(2).pow(c)
      }
      val errHigh = (mAWidth - nPPTrns until mAWidth).foldLeft(BigDecimal(0.0)) { case (acc, c) =>
        acc + 0.25 * scala.math.max(0, dotCount(c, aW, bW) - 2) * BigDecimal(2).pow(c)
      }
      ((errLow + errHigh) / 2).toBigInt.U(pW.W)
    }
    val corr = WireDefault(corrConsts(0))
    modeDists.zipWithIndex.drop(1).foreach { case (_, mInd) =>
      when(io.ctrl === mInd.U) {
        corr := corrConsts(mInd)
      }
    }

    // Depending on the parameters passed, generate a naive adder tree or use 
    // the custom compressor tree generator
    if (comp) {
      // Instantiate a compressor tree and input the bits
      // (incl. sign-extension constant)
      val sig = {
        // Get a plain radix-2 signature
        val multSig = (new MultSignature(aW, bW, true, true)).signature
        // Add the error compensation bits
        (0 until multSig.size).foreach { col => if (col < pW) multSig(col) += 1 }
        new Signature(multSig)
      }
      val comp = Module(CompressorTree(sig, targetDevice=targetDevice))
      val ins = Wire(Vec(sig.count, Bool()))
      var offset = 0
      (0 until sig.outW).foreach { col =>
        // Add the partial product bits
        val low  = lsCol(col, bW)
        val high = low + dotCount(col, aW, bW)
        (low until high).foreach { row =>
          ins(offset) := mskdPprods(row)(col)
          offset += 1
        }

        // Add the error compensation bit
        if (col < pW) {
          ins(offset) := corr(col)
          offset += 1
        }

        // Add the sign-extension bits
        if (col == midLow) {
          ins(offset) := true.B
          offset += 1
        }
        if (col == midHigh) {
          ins(offset) := true.B
          offset += 1
        }
        if (col == upper) {
          ins(offset) := true.B
          offset += 1
        }
      }
      comp.io.in := ins.asUInt
      io.p := comp.io.out
    } else {
      // Compute the sign-extension constant
      val extConst = (BigInt(1) << midLow) + (BigInt(1) << midHigh) + (BigInt(1) << upper)

      // Sum all the partial products and the constant
      io.p := VecInit(mskdPprods.map(_.asUInt) :+ corr :+ extConst.U)
        .reduceTree(_ +& _)
    }
  } else {
    // ... both operands are unsigned
    val (aW, opA) = (aWidth, io.a)
    val (bW, opB) = (bWidth, io.b)
    val  pW = aW + bW

    // Create all the partial products
    val pprods = (0 until aW).map { i => VecInit(opB.asBools.map(_ & opA(i))).asUInt }

    // Create all the shifted partial products
    val shftPprods = WireDefault(VecInit(pprods.zipWithIndex.map { case (pprod, ind) =>
      VecInit((0.U((pW-bW-ind).W) ## (if (ind == 0) pprod else (pprod ## 0.U(ind.W)))).asBools) }))

    // Apply the transmission bits to the shifted partial products
    val mskdPprods = WireDefault(shftPprods)
    modeDists.zipWithIndex.drop(1).foreach { case (mAWidth, mInd) =>
      when(io.ctrl === mInd.U) {
        // Apply the least significant transmission bit to some least 
        // significant columns
        (0 until mAWidth - nPPTrns).foreach { c =>
          val low  = lsCol(c, bW)
          val high = low + dotCount(c, aW, bW)
          (low until high).foreach { r =>
            mskdPprods(r)(c) := shftPprods(r)(c) & ppTrns(0) }
        }

        // Introduce some error correction in the more significant columns
        (mAWidth - nPPTrns until mAWidth).zipWithIndex.foreach { case (c, ind) =>
          val low  = lsCol(c, bW)
          val high = low + dotCount(c, aW, bW)
          (low until high - 2).foreach { r =>
            mskdPprods(r)(c) := shftPprods(r)(c) &  ppTrns(ind) }
          (scala.math.max(0, high - 2) until high).foreach { r =>
            mskdPprods(r)(c) := shftPprods(r)(c) | !ppTrns(ind) }
        }
      }
    }

    // Compute some error compensation constants
    val corrConsts = Seq(0.U(pW.W)) ++ modeDists.drop(1).map { mAWidth =>
      val errLow = (0 until mAWidth - nPPTrns).foldLeft(BigDecimal(0.0)) { case (acc, c) =>
        acc + 0.25 * dotCount(c, aW, bW) * BigDecimal(2).pow(c)
      }
      val errHigh = (mAWidth - nPPTrns until mAWidth).foldLeft(BigDecimal(0.0)) { case (acc, c) =>
        acc + 0.25 * scala.math.max(0, dotCount(c, aW, bW) - 2) * BigDecimal(2).pow(c)
      }
      ((errLow + errHigh) / 2).toBigInt.U(pW.W)
    }
    val corr = WireDefault(corrConsts(0))
    modeDists.zipWithIndex.drop(1).foreach { case (_, mInd) =>
      when(io.ctrl === mInd.U) {
        corr := corrConsts(mInd)
      }
    }

    // Depending on the parameters passed, generate a naive adder tree or use 
    // the custom compressor tree generator
    if (comp) {
      // Instantiate a compressor tree and input the bits
      // (incl. sign-extension constant)
      val sig = {
        // Get a plain radix-2 signature
        val multSig = (new MultSignature(aW, bW, false, false)).signature
        // Add the error compensation bits
        (0 until multSig.size).foreach { col => if (col < pW) multSig(col) += 1 }
        new Signature(multSig)
      }
      val comp = Module(CompressorTree(sig, targetDevice=targetDevice))
      val ins = Wire(Vec(sig.count, Bool()))
      var offset = 0
      (0 until sig.outW).foreach { col =>
        // Add the partial product bits
        val low  = lsCol(col, bW)
        val high = low + dotCount(col, aW, bW)
        (low until high).foreach { row =>
          ins(offset) := mskdPprods(row)(col)
          offset += 1
        }

        // Add error compensation bit
        if (col < pW) {
          ins(offset) := corr(col)
          offset += 1
        }
      }
      comp.io.in := ins.asUInt
      io.p := comp.io.out
    } else {
      // Sum all the partial products
      io.p := VecInit(mskdPprods.map(_.asUInt) :+ corr).reduceTree(_ +& _)
    }
  }
}

object AdaptiveRadix2Multiplier extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new AdaptiveRadix2Multiplier(14, 7, 5, comp=true, numModes=2), Array("--target-dir", "build"))
}
