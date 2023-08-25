package approx.multiplication

import chisel3.Bool

import scala.collection.mutable

package object comptree {

  /** Compressor approximation styles */
  abstract class Approximation
  case class NoApproximation() extends Approximation
  case class Truncation(width: Int) extends Approximation
  case class Miscounting(width: Int) extends Approximation

  /** Compressor tree signature
   * 
   * @param signature array of integers denoting the number of bits per column
   */
  class Signature(val signature: Array[Int]) {
    /** Return the length of the signature */
    def length: Int = signature.length

    /** Return a particular element of the signature */
    def apply(log2Weight: Int): Int = signature(log2Weight)

    /** Return this signature in a string representation */
    override def toString(): String = signature.mkString(",")

    /** Compute the signature's total number of inputs */
    lazy val count: Int = signature.sum

    /** Compute the signature's needed output width */
    lazy val outW: Int = {
      // Use BigInts to accumulate over the signature
      var sum = signature.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (count, log2Weight)) =>
        acc + Array.fill(count) { BigInt(0).setBit(log2Weight) }.sum
      }

      // Return the MSB's position
      var pos = 0
      while (sum > 0) {
        pos  += 1
        sum >>= 1
      }
      pos
    }
  }

  /** Compressor tree string signature
   * 
   * @param signature string of comma-separated integers denoting the number of
   *                  bits per column
   */
  class StringSignature(signature: String) extends Signature(signature.split(",").map(_.toInt))

  /** Multiplier compressor tree signature
   * 
   * @param aW the width of the first operand
   * @param bW the width of the second operand
   * @param aSigned whether the first operand is signed (defaults to false)
   * @param bSigned whether the second operand is signed (defaults to false)
   */
  class MultSignature(aW: Int, bW: Int, aSigned: Boolean = false, bSigned: Boolean = false)
    extends Signature(MultSignature.genSignature(!(aSigned || bSigned), aW, bW))
  private[comptree] object MultSignature {
    /** Generate the signature for a generic (un-)signed by (un-)signed
     * integer multiplier
     * 
     * @param unsigned whether both input operands are unsigned
     * @param aW the width of the first operand
     * @param bW the width of the second operand
     * @return an array-formatted signature of the multiplier
     */
    def genSignature(unsigned: Boolean, aW: Int, bW: Int): Array[Int] = {
      val midLow  = scala.math.min(aW, bW) - 1
      val midHigh = scala.math.max(aW, bW) - 1
      val upper   = aW + bW - 1
      // Compute and insert the number of bits in each column of the signature
      val pprods = (0 until aW + bW - (if (unsigned) 1 else 0)).foldLeft(Array.empty[Int]) { case (acc, col) =>
        val numBits = if (col < midLow) col + 1
          else if (midLow <= col && col <= midHigh) (if (aW > bW) bW else aW)
          else if (col < upper) aW + bW - col - 1
          else 0
        acc :+ numBits
      }
      // Insert the sign-extension bits as needed
      if (!unsigned) {
        pprods(midLow)  += 1
        pprods(midHigh) += 1
        pprods(upper)   += 1
      }
      pprods
    }
  }

  /** Counter fitness metrics */
  private[comptree] object FitnessMetric extends Enumeration {
    type FitnessMetric = Value
    val Strength   = Value("strength")
    val Efficiency = Value("efficiency")
  }
  import FitnessMetric._

  /** Compressor generation state
   * 
   * Used to track how many counters are used in each stage, as needed
   * when generating compressors for FPGAs.
   */
  private[comptree] class State {
    var stages:   Int = 0
    var counters: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int]

    /** Add a stage */
    def addStage(): Unit = stages += 1

    /** Add a counter to this stage */
    def addCounter(): Unit = {
      counters.addAll(Array.fill(stages - counters.length + 1)(0))
      counters(stages) += 1
    }
  }

  /** Compressor generation context
   * 
   * @param outW the targeted output width of the compressor tree
   * @param targetDevice the device to target generation for (if left empty, picks ASIC,
   *                     can also be one of "7series", "ultrascale", "versal", "intel")
   * @param approx the type of approximation to use in the compressor tree
   * @param mtrc which fitness metric to use for selecting counters
   * 
   * Used to control generation compressor tree generation, including
   * the selection of counters, the compression goal, and the final adder.
   */
  private[comptree] class Context(val outW: Int, targetDevice: String, val approx: Approximation, mtrc: Char) {
    private val _device: String = targetDevice.toLowerCase()
    private val _mtrc: Char = mtrc.toLower
    val goal: Int = _device match {
      case "7series" | "ultrascale" => 3
      case _ => 2
    }
    val metric = _mtrc match {
      case 's' => Strength
      case 'e' => Efficiency
      case _ =>
        throw new IllegalArgumentException(s"unsupported fitness metric ${_mtrc}, must be one of 'e' (efficiency) or 's' (strength)")
    }
    val counters = _device match {
      case "7series" | "ultrascale" => Counters.SevenSeries
      case "versal" => Counters.Versal
      case "intel" => Counters.Intel
      case _ => Counters.ASIC
    }
  }

  /** Bit matrix class */
  private[comptree] class BitMatrix {
    // Stack-based bit matrix storage
    val bits = mutable.ArrayBuffer.empty[mutable.Stack[Bool]]

    /** Return the length of the matrix */
    def length: Int = bits.length

    /** Compute the matrix's total number of bits */
    def count: Int = bits.foldLeft(0) { case (acc, col) => acc + col.size }

    /** Compute the matrix's number of bits in a given column
     * 
     * Returns 0 for columns that do not exist.
     */
    def colCount(col: Int): Int = if (bits.isDefinedAt(col)) bits(col).size else 0

    /** Insert a bit into the matrix
     * 
     * @param bit the bit to add
     * @param log2Weight the bit's index
     */
    def insertBit(bit: Bool, log2Weight: Int): Unit = {
      // Extend the bit matrix to the expected length
      bits.addAll(Array.fill(log2Weight - bits.length + 1) { mutable.Stack.empty[Bool] })

      // Now insert the bit
      bits(log2Weight).push(bit)
    }

    /** Pop and return a bit from the matrix
     * 
     * @param log2Weight the bit's index
     */
    def popBit(log2Weight: Int): Bool = {
      assume(bits.length > log2Weight && bits(log2Weight).nonEmpty,
        s"bit matrix with ${bits.length} columns cannot pop bit from column ${log2Weight}")
      bits(log2Weight).pop()
    }

    /** Check if the bit matrix meets a compression goal
     * 
     * @param goal the compression goal in bits
     */
    def meetsGoal(goal: Int): Boolean = bits.forall(_.size <= goal)
  }
}
