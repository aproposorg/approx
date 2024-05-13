package approx.multiplication

import chisel3._
import chisel3.util.log2Up

/** Adaptive radix 2 combinational multiplier IO bundle
 * 
 * @param aWidth the width of the first operand
 * @param bWidth the width of the second operand
 * @param numModes the number of approximation modes
 */
class AdaptiveMultiplierIO(aW: Int, bW: Int, numModes: Int) extends MultiplierIO(aW, bW) {
  val ctrl = Input(UInt(log2Up(numModes+1).W))
}

/** Adaptive radix 2 combinational multiplier IO bundle
 * 
 * @param aWidth the width of the first operand
 * @param bWidth the width of the second operand
 * @param approxWidth the width of the approximate part
 *                    (equal to the number of columns to approximate)
 * @param aSigned whether the first operand is signed (defaults to false)
 * @param bSigned whether the second operand is signed (defaults to false)
 * @param numModes the number of approximation modes (defaults to 1)
 * 
 * Does not make use of the compressor tree generator to add partial products yet.
 */
class AdaptiveRadix2Multiplier(val aWidth: Int, val bWidth: Int, val approxWidth: Int,
  val aSigned: Boolean = false, val bSigned: Boolean = false, val numModes: Int = 1)
  extends Module with HasRadix2PartialProducts {
  require(approxWidth <= (aWidth + bWidth),
    "width of the approximate part must be less than or equal to the product width")
  require(numModes >= 1, "number of approximation modes must be positive")

  val io = IO(new AdaptiveMultiplierIO(aWidth, bWidth, numModes))

  // Determine the number of columns to approximate in each mode; 0 meaning 
  // fully exact multiplication
  val modeDists = Seq(0) ++ (1 until numModes).map(m => approxWidth / numModes * m) ++ Seq(approxWidth)

  // Depending on aSigned and bSigned, generate and unsigned or signed multiplier
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

    // Compute the sign-extension constant
    val extConst = (BigInt(1) << midLow) + (BigInt(1) << midHigh) + (BigInt(1) << upper)

    // Create all the shifted partial products and mask their least 
    // significant columns as required by the approximation mode
    val shftPprods = WireDefault(VecInit(pprods.zipWithIndex.map { case (pprod, ind) =>
      VecInit((0.U((pW-bW-ind).W) ## (if (ind == 0) pprod else (pprod ## 0.U(ind.W)))).asBools) }))
    modeDists.zipWithIndex.foreach { case (mAWidth, mInd) =>
      when(io.ctrl === mInd.U) {
        // Mask the mAWidth least significant bits in each partial product
        (0 until mAWidth).foreach { c =>
          (0 until aW).foreach { pp => shftPprods(pp)(c) := false.B }
        }
      }
    }

    // Sum all the partial products and the constant
    io.p := VecInit(shftPprods.map(_.asUInt) :+ extConst.U).reduceTree(_ +& _)
  } else {
    // ... both operands are unsigned
    val (aW, opA) = (aWidth, io.a)
    val (bW, opB) = (bWidth, io.b)
    val  pW = aW + bW

    // Create all the partial products
    val pprods = (0 until aW).map { i => VecInit(opB.asBools.map(_ & opA(i))).asUInt }

    // Create all the shifted partial products and mask their least 
    // significant columns as required by the approximation mode
    val shftPprods = WireDefault(VecInit(pprods.zipWithIndex.map { case (pprod, ind) =>
      VecInit((0.U((pW-bW-ind).W) ## (if (ind == 0) pprod else (pprod ## 0.U(ind.W)))).asBools) }))
    modeDists.zipWithIndex.foreach { case (mAWidth, mInd) =>
      when(io.ctrl === mInd.U) {
        // Mask the mAWidth least significant bits in each partial product
        (0 until mAWidth).foreach { c =>
          (0 until aW).foreach { pp => shftPprods(pp)(c) := false.B }
        }
      }
    }

    // Sum all the partial products
    io.p := VecInit(shftPprods.map(_.asUInt)).reduceTree(_ +& _)
  }
}
