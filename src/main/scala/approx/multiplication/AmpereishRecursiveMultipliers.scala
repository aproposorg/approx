package approx.multiplication

import chisel3._
import chisel3.util._

/** AMPEREISH approximate multiplier
 *
 * @param width the width of the multiplier
 *
 * Implementation of the multiplier of Rasheed et al. [2026]
 *
 * Only works for unsigned numbers.
 */
class Ampereish(width: Int) extends Multiplier(width, width) {
  require(width > 0, "width must be positive")

  /** 2x2 approximate multiplier M1 */
  class M1 extends Kulkarni

  /** 2x2 approximate multiplier M2 */
  class M2 extends TwoXTwoMult {
    private val p0Raw = io.a(0) & io.b(0)
    private val p1    = (io.a(0) & io.b(1)) ^ (io.a(1) & io.b(0))
    private val carry = (io.a(0) & io.b(1)) & (io.a(1) & io.b(0))
    private val p2    = carry ^ (io.a(1) & io.b(1))
    private val p3    = carry & (io.a(1) & io.b(1))

    private val msbApprox = (io.a(1) & io.b(1)) & ((io.a(0) & io.b(1)) | (io.a(1) & io.b(0)))
    private val p0 = p0Raw ^ msbApprox

    io.p := p3 ## p2 ## p1 ## p0
  }

  /** 2x2 approximate multiplier M3 */
  class M3 extends TwoXTwoMult {
    private val exact = Module(new TwoXTwo)
    exact.io.a := io.a
    exact.io.b := io.b

    private val isFull = io.a.andR && io.b.andR
    io.p := Mux(isFull, 11.U, exact.io.p)
  }

  /** 2x2 approximate multiplier M4 */
  class M4 extends ApproxMul4

  /** 2x2 exact multiplier M5 */
  class M5 extends TwoXTwo

  /** Recursive 4-bit multiplier built from four 2-bit (approximate) multiplier blocks.
   *
   * @param hh function to instantiate the AH*BH multiplier
   * @param hl function to instantiate the AH*BL multiplier
   * @param lh function to instantiate the AL*BH multiplier
   * @param ll function to instantiate the AL*BL multiplier
   *
   * The 4x4 product is assembled as:
   *   p = (AH*BH << 4) + (AH*BL << 2) + (AL*BH << 2) + (AL*BL)
   *     = (HH.p  << 4) + (HL.p  << 2) + (LH.p  << 2) + (LL.p)
   */
  private[Ampereish] class Recursive4BitMultiplier(
    val hh: () => TwoXTwoMult,
    val hl: () => TwoXTwoMult,
    val lh: () => TwoXTwoMult,
    val ll: () => TwoXTwoMult)
  extends Multiplier(4, 4) {
    private val aH = io.a(3, 2)
    private val aL = io.a(1, 0)
    private val bH = io.b(3, 2)
    private val bL = io.b(1, 0)

    private val mulHH = Module(hh())
    private val mulHL = Module(hl())
    private val mulLH = Module(lh())
    private val mulLL = Module(ll())

    mulHH.io.a := aH
    mulHH.io.b := bH
    mulHL.io.a := aH
    mulHL.io.b := bL
    mulLH.io.a := aL
    mulLH.io.b := bH
    mulLL.io.a := aL
    mulLL.io.b := bL

    private val zHH = (mulHH.io.p ## 0.U(4.W)).asUInt
    private val zHL = (mulHL.io.p ## 0.U(2.W)).asUInt
    private val zLH = (mulLH.io.p ## 0.U(2.W)).asUInt
    private val zLL = mulLL.io.p

    io.p := zHH +& zHL +& zLH +& zLL
  }

  /** Recursive 8-bit multiplier built from four 4-bit (approximate) multiplier blocks.
   *
   * @param hh function to instantiate the AH*BH multiplier
   * @param hl function to instantiate the AH*BL multiplier
   * @param lh function to instantiate the AL*BH multiplier
   * @param ll function to instantiate the AL*BL multiplier
   *
   * The 8x8 product is assembled as:
   *   p = (AH*BH << 8) + (AH*BL << 4) + (AL*BH << 4) + (AL*BL)
   *     = (HH.p  << 8) + (HL.p  << 4) + (LH.p  << 4) + (LL.p)
   */
  private[Ampereish] class Recursive8BitMultiplier(
    val hh: () => Multiplier,
    val hl: () => Multiplier,
    val lh: () => Multiplier,
    val ll: () => Multiplier)
  extends Multiplier(8, 8) {
    private val aH = io.a(7, 4)
    private val aL = io.a(3, 0)
    private val bH = io.b(7, 4)
    private val bL = io.b(3, 0)

    private val mulHH = Module(hh())
    private val mulHL = Module(hl())
    private val mulLH = Module(lh())
    private val mulLL = Module(ll())

    require(mulHH.aWidth == 4 && mulHH.bWidth == 4)
    require(mulHL.aWidth == 4 && mulHL.bWidth == 4)
    require(mulLH.aWidth == 4 && mulLH.bWidth == 4)
    require(mulLL.aWidth == 4 && mulLL.bWidth == 4)

    mulHH.io.a := aH
    mulHH.io.b := bH
    mulHL.io.a := aH
    mulHL.io.b := bL
    mulLH.io.a := aL
    mulLH.io.b := bH
    mulLL.io.a := aL
    mulLL.io.b := bL

    private val zHH = (mulHH.io.p ## 0.U(8.W)).asUInt
    private val zHL = (mulHL.io.p ## 0.U(4.W)).asUInt
    private val zLH = (mulLH.io.p ## 0.U(4.W)).asUInt
    private val zLL = mulLL.io.p

    io.p := zHH +& zHL +& zLH +& zLL
  }

  // Compute the widths and number of sub-modules
  private val blkW = if (width <= 4) 4 else 8
  private val extW = if (width <= 4) 4 else ((width + 7) / 8) * 8
  private val nBlk = extW / blkW

  // Extend the input operands to fit the block width
  private val aExtended = Wire(UInt(extW.W))
  private val bExtended = Wire(UInt(extW.W))
  if (extW == width) {
    aExtended := io.a
    bExtended := io.b
  } else {
    aExtended := 0.U((extW - width).W) ## io.a
    bExtended := 0.U((extW - width).W) ## io.b
  }

  // Split the operands into blocks and compute the partial products
  private val aBlocks = aExtended.asTypeOf(Vec(nBlk, UInt(blkW.W)))
  private val bBlocks = bExtended.asTypeOf(Vec(nBlk, UInt(blkW.W)))

  private val partialProducts = (0 until nBlk).flatMap { i =>
    (0 until nBlk).map { j =>
      val multiplier = if (blkW == 4) {
        Module(new Recursive4BitMultiplier(() => new M1, () => new M1, () => new M1, () => new M1))
      } else {
        Module(new Recursive8BitMultiplier(
          () => new Recursive4BitMultiplier(() => new M1, () => new M1, () => new M1, () => new M1),
          () => new Recursive4BitMultiplier(() => new M1, () => new M1, () => new M1, () => new M1),
          () => new Recursive4BitMultiplier(() => new M1, () => new M1, () => new M1, () => new M1),
          () => new Recursive4BitMultiplier(() => new M1, () => new M1, () => new M1, () => new M1)
        ))
      }

      multiplier.io.a := aBlocks(i)
      multiplier.io.b := bBlocks(j)
      (multiplier.io.p ## 0.U(((i + j) * blkW).W)).asUInt
    }
  }

  io.p := VecInit(partialProducts).reduceTree(_ +& _)
}
