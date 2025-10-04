package approx.multiplication.comptree

import chisel3._
import chisel3.util.experimental.FlattenInstance

import scala.collection.mutable

import Counters._
import TerminalAdders._

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
 */
private[comptree] class CompressorTree(val sig: Signature, context: Context) extends Module with FlattenInstance {
  private val state = new State()

  // The sum width from the context or the signature
  val outW = if (context.outW == -1) sig.outW else context.outW
  val io = IO(new Bundle {
    val in  = Input(UInt(sig.count.W))
    val out = Output(UInt(outW.W))
  })

  // Select the appropriate sets of regular and variable-length counters
  // and sort them by the desired fitness metric. For the latter, only
  // evaluate these metrics at a length of three, as this seems a good
  // balancing point in Hossfeld et al. [2024]
  val isApprox = context.approximations.exists(_.isInstanceOf[Miscounting])
  private val counters = (if (isApprox) context.counters.approxCounters else context.counters.exactCounters)
    .sortBy { cntr  => context.metric match {
    case FitnessMetric.Efficiency => cntr.efficiency
    case FitnessMetric.Strength   => cntr.strength
  }}.reverse
  private val vlcounters = (if (isApprox) context.counters.approxVarLenCounters else context.counters.exactVarLenCounters)
    .sortBy { cntr  => context.metric match {
    case FitnessMetric.Efficiency => cntr.efficiency(3)
    case FitnessMetric.Strength   => cntr.strength(3)
  }}.reverse

  // Generate and connect the inputs to a bit matrix
  private val inMtrx = buildMatrix(sig, io.in)

  // Iteratively compress the bit matrix till the compression goal is reached
  private val mtrcs = mutable.ArrayBuffer(inMtrx)
  while (!mtrcs.last.meetsGoal(context.goal))
    mtrcs += compress(mtrcs.last, counters, vlcounters)

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
          res.insertBit(bits(ind + signature(log2Weight) - 1, ind).orR, log2Weight)
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
   * @param vlcntrs the appropriate available variable-length counters
   * @return the bit matrix after compression, before final summation
   */
  private[CompressorTree] def compress(bits: BitMatrix, cntrs: Seq[Counter], vlcntrs: Seq[VarLenCounter]): BitMatrix = {
    state.addStage()

    // Place a new counter in the bit matrix while possible
    val res = new BitMatrix()
    while (!bits.meetsGoal(context.goal))
      placeLargestCntr(cntrs, vlcntrs, bits, res)

    // Transfer any remaining bits to the next stage
    transferBits(bits, res)
    res
  }

  /** Place and construct the largest possible counter (measured in strength
   * or efficiency) in a compression stage
   * 
   * @param cntrs the selection of counters to pick from
   * @param vlcntrs the selection of variable-length counters to pick from
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
  private[CompressorTree] def placeLargestCntr(cntrs: Seq[Counter], vlcntrs: Seq[VarLenCounter], inBits: BitMatrix, outBits: BitMatrix): Unit = {
    // Find the least significant column that still needs compression
    val lsColOpt = inBits.bits
      .zipWithIndex
      .collectFirst({ case (stck, i) if stck.size > context.goal => i })

    lsColOpt match {
      case Some(lsCol) =>
        // Pick the best regular and variable-length counters that fit in
        // the bit matrix starting from the least significant column found
        val bestRegCntrOpt = cntrs.collectFirst {
          case rcntr if canPlaceCntr(rcntr, inBits, outBits, lsCol) =>
            val mtrc = context.metric match {
              case FitnessMetric.Efficiency => rcntr.efficiency
              case FitnessMetric.Strength   => rcntr.strength
            }
            (rcntr, mtrc)
        }
        val bestVLCntrOpt = vlcntrs.collectFirst {
          case vlcntr if canPlaceVLCntr(vlcntr, inBits, lsCol) => // search for length 1
            val len  = maxVLCntrLen(vlcntr, inBits, lsCol)
            val mtrc = context.metric match {
              case FitnessMetric.Efficiency => vlcntr.efficiency(len)
              case FitnessMetric.Strength   => vlcntr.strength(len)
            }
            (vlcntr, len, mtrc)
        }

        // Select the best of the two counters
        val bestCntrOpt = (bestRegCntrOpt, bestVLCntrOpt) match {
          case (Some((rcntr, rMtrc)), Some((vlcntr, len, vlMtrc))) =>
            if (rMtrc >= vlMtrc) Some((rcntr, 0)) else Some((vlcntr, len))
          case (Some((rcntr, _)), _) => Some((rcntr, 0))
          case (_, Some((vlcntr, len, _))) => Some((vlcntr, len))
          case _ => None
        }

        // Construct the counter and connect it accordingly
        val (inSig, outSig, hwCntr) = bestCntrOpt match {
          case Some((rcntr: Counter, _)) => // regular counter
            state.addCounter(rcntr)

            val hwCntr = context.counters.construct(rcntr, state)
            val inSig  = rcntr.sig._1
            val outSig = rcntr.sig._2
            (inSig, outSig, hwCntr)

          case Some((vlcntr: VarLenCounter, len: Int)) => // variable-length counter
            state.addVLCounter(vlcntr, len)

            val hwCntr = context.counters.construct(vlcntr, len, state)
            val inSig  = vlcntr.inSigFn(len)
            val outSig = vlcntr.outSigFn(len)
            (inSig, outSig, hwCntr)

          case _ => throw new Exception("cannot place a counter in input bit matrix")
        }

        // ... inputs first
        hwCntr.io.in := VecInit((0 until inSig.length).flatMap { col =>
          (0 until inSig(col)).map(_ => inBits.popBit(col + lsCol))
        }.reverse).asUInt

        // ... outputs second
        var index = 0
        (0 until outSig.length).foreach { col =>
          (0 until outSig(col)).foreach { _ =>
            outBits.insertBit(hwCntr.io.out(index), col + lsCol)
            index += 1
          }
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

  /** Check whether a particular variable-length counter fits in an input
   * bit matrix starting from the given column
   * 
   * @param vlcntr the variable-length counter
   * @param inBits the input bit matrix
   * @param lsCol the least significant (starting) column
   * @return true if the counter fits, false otherwise
   */
  private[CompressorTree] def canPlaceVLCntr(vlcntr: VarLenCounter, inBits: BitMatrix, lsCol: Int): Boolean = {
    val inSig = vlcntr.inSigFn(1) // search for length 1
    inSig.zipWithIndex.forall { case (cnt, col) => inBits.colCount(lsCol + col) >= cnt }
  }

  /** Determine the maximum length of a variable-length counter that
   * can be placed in an input bit matrix starting from the given column
   * 
   * @param vlcntr the variable-length counter
   * @param inBits the input bit matrix
   * @param lsCol the least significant (starting) column
   * @return the maximum length of the variable-length counter that can be placed
   */
  private[CompressorTree] def maxVLCntrLen(vlcntr: VarLenCounter, inBits: BitMatrix, lsCol: Int): Int = {
    /** Binary search to find the maximum permissible length */
    def search(low: Int, high: Int, maxLen: Int): Int = {
      if (low >= high) {
        maxLen
      } else {
        val mid   = (low + high) / 2
        val inSig = vlcntr.inSigFn(mid)
        val fits  = inSig.zipWithIndex.forall { case (cnt, col) =>
          inBits.colCount(lsCol + col) >= cnt
        }

        if (fits) search(mid + 1, high, mid)
        else search(low, mid, maxLen)
      }
    }

    /** Default to length 1 as this function is called after `canPlaceVLCntr`.
     * Maximum length is constrained either by the width of the input matrix
     * or the height of its tallest column
     */
    val lowInit  = 2
    val highInit = scala.math.max(inBits.length - lsCol, inBits.bits.map(_.size).max)
    search(lowInit, highInit, 1)
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
   */
  private[CompressorTree] def finalSummation(bits: BitMatrix): UInt = {
    require((0 until outW).forall(i => bits.colCount(i) <= context.goal))

    // Add false bits to all the columns with less than `goal` bits
    for (i <- 0 until outW) {
      (bits.colCount(i) until context.goal).foreach(_ => bits.insertBit(false.B, i))
    }

    // Collect the operands as integers
    val oprs = WireDefault(VecInit((0 until context.goal).map { _ =>
      val op = Wire(Vec(outW, Bool()))
      (0 until outW).foreach(i => op(i) := bits.popBit(i))
      op.asUInt
    }))

    // Depending on the target device, instantiate a different adder
    context.terminal match {
      case "ternary" =>
        val adder = Module(new TernaryAdder(outW))
        adder.io.in := oprs
        adder.io.out
      case "quaternary" =>
        val adder = Module(new QuaternaryAdder(outW))
        adder.io.in := oprs
        adder.io.out
      case _ =>
        // Default to a simple binary adder tree
        oprs.reduceTree(_ + _)
    }
  }
}
