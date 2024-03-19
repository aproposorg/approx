package approx.multiplication.comptree

import chisel3._
import chisel3.util.experimental.FlattenInstance

import scala.collection.mutable

import Counters._

/** Compressor tree generator object */
object CompressorTree {
  /** Generate a compressor tree
   * 
   * @param sig the compressor tree's input signature
   * @param outW the targeted output width (defaults to the minimum needed by the 
   *             input signature)
   * @param targetDevice a string indicating the target device
   *                     (defaults to "", meaning ASIC)
   * @param mtrc which metric to use for selecting counters (defaults to efficiency)
   * @param approx the targeted approximation styles (defaults to no approximation)
   */
  def apply[T <: Signature](sig: T, outW: Int = -1, targetDevice: String = "",
                            mtrc: Char = 'e', approx: Seq[Approximation] = Seq.empty[Approximation]): CompressorTree =
    new CompressorTree(sig, new Context(outW, targetDevice, mtrc, approx))
}

/** Compressor tree class
 * 
 * @param sig the compressor tree's input signature
 * @param context the generation context
 * 
 * Not expected to be manually instantiated by users. Instead, one may rely
 * on the companion object for a simplified interface to this generator.
 * 
 * @todo Update to make use of the state in LUT placement and pipelining.
 */
private[comptree] class CompressorTree(val sig: Signature, context: Context) extends Module with FlattenInstance {
  val state = new State()

  // The sum width from the context or the signature
  val outW = if (context.outW == -1) sig.outW else context.outW
  val io = IO(new Bundle {
    val in  = Input(UInt(sig.count.W))
    val out = Output(UInt(outW.W))
  })

  // Select the appropriate set of counters and sort them by the desired 
  // fitness metric
  val isApprox = context.approximations.exists(_.isInstanceOf[Miscounting])
  val cntrs = (if (isApprox) context.counters.approxCounters else context.counters.exactCounters)
    .sortBy { cntr  => context.metric match {
    case FitnessMetric.Efficiency => cntr.efficiency
    case FitnessMetric.Strength   => cntr.strength
  }}.reverse

  // Generate and connect the inputs to a bit matrix
  val inMtrx = buildMatrix(sig, io.in)

  // Iteratively compress the bit matrix till the compression goal is reached
  val mtrcs = mutable.ArrayBuffer(inMtrx)
  while (!mtrcs.last.meetsGoal(context.goal)) mtrcs += compress(mtrcs.last, cntrs)

  // Perform a final summation and output the result
  io.out := finalSummation(mtrcs.last)

  /** Construct a bit matrix and connect the given bits to it
   * 
   * @param signature the signature of the matrix
   * @param bits the flattened bit vector to connect to the matrix
   * @return a bit matrix with the given signature and the given bits connected
   */
  private[CompressorTree] def buildMatrix(signature: Signature, bits: UInt): BitMatrix = {
    require(signature.count == bits.getWidth)
    val res = new BitMatrix()

    // If the compressor tree implements column truncation or OR compression,
    // skip or compute the input bits for the columns in question here
    var ind = 0
    val startLog2Weight = {
      // First apply column truncation
      val ctWidth = context.approximations.collect { case ct: ColumnTruncation =>
        ind += (0 until ct.width).map(i => signature(i)).sum
        ct.width
      }.headOption.getOrElse(0)

      // Then apply OR compression
      val orWidth = context.approximations.collect { case or: ORCompression =>
        for (log2Weight <- ctWidth until or.width) {
          res.insertBit(bits(ind + signature(log2Weight) - 1, ind).orR(), log2Weight)
          ind += signature(log2Weight)
        }
        or.width
      }.headOption.getOrElse(0)

      // Return the maximum index
      scala.math.max(ctWidth, orWidth)
    }

    // If the compressor tree implements row truncation, generate the signature
    // of the bits to truncate here
    val truncSig = context.approximations.collect { case rt: RowTruncation =>
      signature match {
        case multSig: MultSignature =>
          val tSig = new MultSignature(rt.rows, multSig.bW, radix=multSig.radix)
          new Signature(tSig.signature :++ Array.fill(multSig.length - tSig.length)(0))
        case _ =>
          throw new Exception("can only apply row truncation to multiplier signatures")
      }
    }.headOption.getOrElse(new Signature(Array.fill(sig.length)(0)))

    // Insert the remaining bits in the compressor tree
    for (log2Weight <- startLog2Weight until signature.length) {
      // Skip some rows given by the truncated signature
      for (row <- truncSig(log2Weight) until signature(log2Weight)) {
        res.insertBit(bits(ind), log2Weight)
        ind += 1
      }
    }
    res
  }

  /** Recursively schedule and construct compression stages till the
   * compression goal is met
   * 
   * @param bits the input bit matrix
   * @param cntrs the appropriate available counters
   * @return the bit matrix after compression, before final summation
   */
  private[CompressorTree] def compress(bits: BitMatrix, cntrs: Seq[Counter]): BitMatrix = {
    state.addStage()

    // Place a new counter in the bit matrix while possible
    val res = new BitMatrix()
    while (!bits.meetsGoal(context.goal)) placeLargestCntr(cntrs, bits, res)

    // Transfer any remaining bits to the next stage
    transferBits(bits, res)
    res
  }

  /** Place and construct the largest possible counter (measured in strength
   * or efficiency) in a compression stage
   * 
   * @param cntrs the selection of counters to pick from
   * @param inBits the current bit matrix (will be updated)
   * @param outBits the output bit matrix (will be updated)
   * 
   * Does not currently take into account that "phantom" bits (i.e., constant
   * low bits) can be inserted arbitrarily in the matrix. Also does not consider
   * duplication of more significant bits.
   * 
   * Half adders are only placed when their placement "finishes" the compression
   * process of a column (i.e., if its placement brings a column's bit count
   * below the compression goal)
   */
  private[CompressorTree] def placeLargestCntr(cntrs: Seq[Counter], inBits: BitMatrix, outBits: BitMatrix): Unit = {
    state.addCounter()

    // Find the least significant column that still needs compression
    val lsColOpt = inBits.bits
      .zipWithIndex
      .collectFirst({ case (stck, i) if stck.size > context.goal => i })

    lsColOpt match {
      case Some(lsCol) =>
        // Pick the best counter that fits in the bit matrix starting from
        // the found, least significant column
        val bestCntrOpt = cntrs.collectFirst {
          case cntr if canPlaceCntr(cntr, inBits, outBits, lsCol) => cntr
        }

        // Construct the counter and connect it accordingly
        bestCntrOpt match {
          case Some(cntr) =>
            val hwCntr = context.counters.construct(cntr)

            // ... inputs first
            hwCntr.io.in := VecInit((0 until cntr.sig._1.length).flatMap { col =>
              (0 until cntr.sig._1(col)).map(_ => inBits.popBit(col + lsCol))
            }.reverse).asUInt

            // ... outputs second
            var index = 0
            (0 until cntr.sig._2.length).foreach { col =>
              (0 until cntr.sig._2(col)).foreach { _ =>
                outBits.insertBit(hwCntr.io.out(index), col + lsCol)
                index += 1
              }
            }

          case _ => throw new Exception("cannot place a counter in input bit matrix")
        }

      case _ => throw new Exception("input bit matrix does not need compression")
    }
  }

  /** Check whether a particular counter fits in an input bit matrix
   * starting from the given column
   * 
   * @param cntr the counter
   * @param inBits the input bit matrix
   * @param outBits the output bit matrix
   * @param lsCol the least significant (starting) column
   * @return true if the counter fits, false otherwise
   * 
   * The method uses the output bit matrix to judge whether a half adder
   * is currently placeable.
   */
  private[CompressorTree] def canPlaceCntr(cntr: Counter, inBits: BitMatrix, outBits: BitMatrix, lsCol: Int): Boolean = {
    val inSig  = cntr.sig._1

    // If the current counter is a half-adder, check if it will complete
    // the compression of this column
    val isHA = inSig.length == 1 && inSig(0) == 2
    if (isHA) {
      val (inCnt, outCnt) = (inBits.colCount(lsCol), outBits.colCount(lsCol))
      inCnt >= 2 && (inCnt + outCnt - 1 <= context.goal)
    } else {
      // Otherwise, take approximate counters into account too
      context.approximations.collect { case mscnt: Miscounting if isApproximate(cntr) =>
        inSig.zipWithIndex.forall { case (cnt, col) =>
          (lsCol + col) < mscnt.width && inBits.colCount(lsCol + col) >= cnt }
      }.headOption.getOrElse {
        inSig.zipWithIndex.forall { case (cnt, col) =>
          inBits.colCount(lsCol + col) >= cnt }
      }
    }
  }

  /** Check if the stacked pair of an input and an output bit matrix
   * meet the compression goal
   * 
   * @param inBits the first bit matrix
   * @param outBits the second bit matrix
   * @return whether the stacked pair of bit matrices meet the compression goal
   */
  private[CompressorTree] def meetGoal(inBits: BitMatrix, outBits: BitMatrix): Boolean = {
    val maxLen = if (inBits.length > outBits.length) inBits.length else outBits.length
    (0 until maxLen).foldLeft(false) { case (acc, col) =>
      acc || (inBits.colCount(col) + outBits.colCount(col) > context.goal)
    }
  }

  /** Transfer bits from one bit matrix to another
   * 
   * @param inBits the current bit matrix (will be updated)
   * @param outBits the output bit matrix (will be updated)
   */
  private[CompressorTree] def transferBits(inBits: BitMatrix, outBits: BitMatrix): Unit = {
    inBits.bits.zipWithIndex.foreach { case (stck, col) =>
      (0 until stck.size).foreach { _ => outBits.insertBit(inBits.popBit(col), col) }
    }
    assert(inBits.count == 0, "current bit matrix has not been fully transferred")
  }

  /** Schedule and construct a final carry-propagate adder with the given goal
   * 
   * @param bits the bit matrix to sum
   * @param outW the width of the sum 
   * @return the flattened sum of the bit matrix
   * 
   * For now, this implementation relies on the synthesis tools to remove
   * gates from constant-low bit positions.
   * 
   * @todo Implement ternary and quaternary adders for FPGAs.
   */
  private[CompressorTree] def finalSummation(bits: BitMatrix): UInt = {
    require((0 until outW).forall(i => bits.colCount(i) <= context.goal))

    // Add false bits to all the columns with less than `goal` bits
    for (i <- 0 until outW) {
      (bits.colCount(i) until context.goal).foreach(_ => bits.insertBit(false.B, i))
    }

    // Collect the operands as integers and sum them
    val oprs = WireDefault(VecInit((0 until context.goal).map { _ =>
      val op = Wire(Vec(outW, Bool()))
      (0 until outW).foreach(i => op(i) := bits.popBit(i))
      op.asUInt
    }))
    oprs.reduceTree(_ + _)
  }
}
