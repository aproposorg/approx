package approx.multiplication

import chisel3._
import chisel3.util._

/** AMPEREISH approximate multiplier
 *
 * @param width the width of the multiplier
 * @param cellConfig list of integers specifying cell types for least-significant
 *                   positions (0: exact/M5, 1-4: M1-M4); unspecified positions
 *                   default to exact
 *
 * Implementation of the multiplier of Rasheed et al. [2026]
 *
 * Only works for unsigned numbers.
 */
class Ampereish(width: Int, cellConfig: Seq[Int] = Seq.empty) extends Multiplier(width, width) {
  require(width > 0, "width must be positive")
  require(cellConfig.forall(c => c >= 0 && c <= 4), "cellConfig values must be between 0 and 4")

  /** 2x2 approximate multiplier M1 */
  private[Ampereish] class M1 extends Kulkarni

  /** 2x2 approximate multiplier M2 */
  private[Ampereish] class M2 extends TwoXTwoMult {
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
  private[Ampereish] class M3 extends TwoXTwoMult {
    private val exact = Module(new TwoXTwo)
    exact.io.a := io.a
    exact.io.b := io.b

    private val isFull = io.a.andR && io.b.andR
    io.p := Mux(isFull, 11.U, exact.io.p)
  }

  /** 2x2 approximate multiplier M4 */
  private[Ampereish] class M4 extends ApproxMul4

  /** 2x2 exact multiplier M5 */
  private[Ampereish] class M5 extends TwoXTwo

  /** Generate cell based on type */
  private def getCell(cellType: Int): TwoXTwoMult = cellType match {
    case 1 => new M1
    case 2 => new M2
    case 3 => new M3
    case 4 => new M4
    case _ => new M5 // default to exact
  }

  /** Recursive 4-bit multiplier built from four 2-bit (approximate) multiplier blocks.
   *
   * @param cellConfig sequence of 4 integers specifying cell types (0: exact, 1-4: M1-M4)
   *
   * The 4x4 product is assembled as:
   *   p = (AH*BH << 4) + (AH*BL << 2) + (AL*BH << 2) + (AL*BL)
   *     = (HH.p  << 4) + (HL.p  << 2) + (LH.p  << 2) + (LL.p)
   */
  private[Ampereish] class Recursive4BitMultiplier(cellConfig: Seq[Int] = Seq(0,0,0,0))
    extends Multiplier(4, 4) {
    private val paddedConfig = cellConfig.padTo(4, 0)
    private val aH = io.a(3, 2)
    private val aL = io.a(1, 0)
    private val bH = io.b(3, 2)
    private val bL = io.b(1, 0)

    private val mulHH = Module(getCell(paddedConfig(0)))
    private val mulHL = Module(getCell(paddedConfig(1)))
    private val mulLH = Module(getCell(paddedConfig(2)))
    private val mulLL = Module(getCell(paddedConfig(3)))

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
   * @param cellConfig sequence of 16 integers specifying cell types (0: exact, 1-4: M1-M4)
   *
   * The 8x8 product is assembled as:
   *   p = (AH*BH << 8) + (AH*BL << 4) + (AL*BH << 4) + (AL*BL)
   *     = (HH.p  << 8) + (HL.p  << 4) + (LH.p  << 4) + (LL.p)
   */
  private[Ampereish] class Recursive8BitMultiplier(cellConfig: Seq[Int] = Seq.fill(16)(0))
    extends Multiplier(8, 8) {
    private val paddedConfig = cellConfig.padTo(16, 0)
    private val subConfigs = (0 until 4).map(k => paddedConfig.slice(k*4, (k+1)*4))
    private val aH = io.a(7, 4)
    private val aL = io.a(3, 0)
    private val bH = io.b(7, 4)
    private val bL = io.b(3, 0)

    private val mulHH = Module(new Recursive4BitMultiplier(subConfigs(0)))
    private val mulHL = Module(new Recursive4BitMultiplier(subConfigs(1)))
    private val mulLH = Module(new Recursive4BitMultiplier(subConfigs(2)))
    private val mulLL = Module(new Recursive4BitMultiplier(subConfigs(3)))

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

  // Compute total number of 2x2 cells and pad the config
  private val totalCells    = (extW / 2) * (extW / 2)
  private val paddedConfig  = cellConfig.padTo(totalCells, 0)
  private val cellsPerBlock = (blkW / 2) * (blkW / 2)

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
      val configStart = (i * nBlk + j) * cellsPerBlock
      val subConfig   = paddedConfig.slice(configStart, configStart + cellsPerBlock)
      val multiplier  = if (blkW == 4) {
        Module(new Recursive4BitMultiplier(subConfig))
      } else {
        Module(new Recursive8BitMultiplier(subConfig))
      }

      multiplier.io.a := aBlocks(i)
      multiplier.io.b := bBlocks(j)
      (multiplier.io.p ## 0.U(((i + j) * blkW).W)).asUInt
    }
  }

  io.p := VecInit(partialProducts).reduceTree(_ +& _)
}
