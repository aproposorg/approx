package approx.multiplication.comptree

import chisel3._

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Common test patterns for compressor trees
 * 
 * We use the same signatures proposed by Preusser [2017] and some
 * randomly generated ones.
 */
trait CompressorTreeSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  // The tests need an RNG to generate random inputs
  val rng = new scala.util.Random(42)

  // The signatures we test for here
  val Signatures = Seq(
    new StringSignature("128"),
    new StringSignature("256"),
    new StringSignature("512"),
    new StringSignature("128,128"),
    new StringSignature("256,256"),
    new StringSignature("512,512"),
    new MultSignature(16, 16),
    new MultSignature(16, 16, true, true, 4)
  ) ++ (0 until 5).map { _ =>
    new Signature(Array.fill(rng.between(1, 33)) { rng.between(1, 33) })
  }

  // Number of random input vectors to use in the tests
  val NoInputVecs = 10000

  /** For testing purposes, we construct another bit matrix class */
  class BitMatrix[T <: Signature](sig: Signature) {
    // Array-based bit matrix storage
    private val bits: Array[Array[Boolean]] = sig.signature.map(cnt => Array.fill(cnt)(false))

    /** Set the given bit position
     * 
     * @param col the column index of the bit position
     * @param row the row index of the bit position
     */
    def setBit(col: Int, row: Int): Unit = if (bits.isDefinedAt(col) && bits(col).isDefinedAt(row)) bits(col)(row) = true

    /** Set all bits */
    def set(): Unit = (0 until bits.size).foreach { col =>
      (0 until bits(col).size).foreach(row => bits(col)(row) = true)
    }

    /** CLear the given bit position
     * 
     * @param col the column index of the bit position
     * @param row the row index of the bit position
     */
    def clearBit(col: Int, row: Int): Unit = if (bits.isDefinedAt(col) && bits(col).isDefinedAt(row)) bits(col)(row) = false

    /** Clear all bits */
    def clear(): Unit = (0 until bits.size).foreach { col =>
      (0 until bits(col).size).foreach(row => bits(col)(row) = false)
    }

    /** Randomize the bits stored in the matrix */
    def randomize(): Unit = {
      sig.signature.zipWithIndex.foreach { case (cnt, col) => (0 until cnt).foreach { row =>
        bits(col)(row) = rng.nextBoolean()
      }}
    }

    /** Flatten the bits stored in the matrix into a UInt
     * 
     * @return a flattened version of the bit matrix
     */
    def flat: UInt = {
      val bitString = bits.map(_.reverse).reverse.flatten.map(if (_) 1 else 0).mkString
      s"b$bitString".U
    }

    /** Sum and collapse the bits stored in the matrix into a UInt
     * 
     * @return the sum of the bits in the matrix
     */
    def sum: UInt = bits.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (col, wght)) =>
      acc + BigInt(col.count(p => p)) * (BigInt(1) << wght)
    }.U
  }

  /** Poke an input value and check for a given value
   * 
   * @param in the input operand
   * @param out the expected output
   */
  def pokeAndExpect(in: UInt)(out: UInt)(implicit dut: CompressorTree) = {
    dut.io.in.poke(in)
    dut.clock.step()
    dut.io.out.expect(out)
  }

  /** Simple edge case tests
   * 
   * @param dut a compressor module
   */
  def simpleTest(implicit dut: CompressorTree) = {
    val sig = dut.sig

    // Create a bit matrix that matches the compressor tree's input shape
    val bm = new BitMatrix(sig)

    // Set inputs low
    bm.clear()
    pokeAndExpect(bm.flat)(bm.sum)

    // Simple addition tests with single set input bits
    for (col <- (0 until sig.signature.size)) {
      for (row <- (0 until sig.signature(col))) {
        bm.setBit(col, row)
        pokeAndExpect(bm.flat)(bm.sum)
        bm.clearBit(col, row)
      }
    }

    // Set inputs high
    bm.set()
    pokeAndExpect(bm.flat)(bm.sum)
  }

  /** Random input compression tests
   * 
   * @param dut a compressor module
   */
  def randomTest(implicit dut: CompressorTree) = {
    val sig = dut.sig

    // Create a bit matrix that matches the compressor tree's input shape
    val bm = new BitMatrix(sig)

    // Set inputs low
    bm.clear()
    pokeAndExpect(bm.flat)(bm.sum)

    // Randomize the matrix a number of times and check that its result is correct
    for (_ <- 0 until NoInputVecs) {
      bm.randomize()
      pokeAndExpect(bm.flat)(bm.sum)
    }
  }
}

/** Test the compressor generator for various device types
 * 
 * Beware that only compressors generated for ASIC are simulated here
 * due to a lack of device-specific files for the others!
 */
class ASICTreeSpec extends CompressorTreeSpec {
  behavior of "ASIC compressor"
  val targetDevice = "asic"

  for (sig <- Signatures) {
    /** Test simple generation of compressors */
    it should s"generate with signature $sig" in {
      test(CompressorTree(sig, targetDevice=targetDevice)) { dut =>
        dut.io.in.poke(0.U)
        dut.clock.step()
        dut.io.out.expect(0.U)
      }
    }
    it should s"generate with signature $sig and ColumnTruncation(${sig.length/2})" in {
      getVerilogString(CompressorTree(sig, targetDevice=targetDevice, approx=Seq(ColumnTruncation(sig.length/2))))
    }
    it should s"generate with signature $sig and Miscounting(${sig.length/2})" in {
      getVerilogString(CompressorTree(sig, targetDevice=targetDevice, approx=Seq(Miscounting(sig.length/2))))
    }
    it should s"generate with signature $sig and ORCompression(${sig.length/2})" in {
      getVerilogString(CompressorTree(sig, targetDevice=targetDevice, approx=Seq(ORCompression(sig.length/2))))
    }
    sig match {
      case _: MultSignature =>
        it should s"generate with signature $sig and RowTruncation(4)" in {
          getVerilogString(CompressorTree(sig, targetDevice=targetDevice, approx=Seq(RowTruncation(4))))
        }
      case _ =>
        it should s"fail to generate with signature $sig and RowTruncation(4)" in {
          an [Exception] should be thrownBy(getVerilogString(CompressorTree(sig, targetDevice=targetDevice, approx=Seq(RowTruncation(4)))))
        }
    }

    /** Test simple generation of compressors with multiple  */
    val approxs = (sig match {
      case _: MultSignature => Seq(RowTruncation(2))
      case _ => Seq()
    }) ++ Seq(ColumnTruncation(sig.length/8), Miscounting(sig.length/2), ORCompression(sig.length/4))
    for (pair <- approxs.flatMap(one => approxs.filter(_ != one).map(two => Seq(one, two)))) {
      it should s"generate with signature $sig and approximations ${pair.mkString("[", ", ", "]")}" in {
        getVerilogString(CompressorTree(sig, targetDevice=targetDevice, approx=pair))
      }
    }

    /** Test with some simple edge case inputs */
    it should s"function properly with signature $sig at edge case inputs" in {
      test(CompressorTree(sig, targetDevice=targetDevice))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        simpleTest(dut)
      }
    }

    /** Test with some random inputs */
    it should s"function properly with signature $sig given random inputs" in {
      test(CompressorTree(sig, targetDevice=targetDevice))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        randomTest(dut)
      }
    }
  }
}

class SevenSeriesTreeSpec extends CompressorTreeSpec {
  behavior of "7 Series compressor"
  val targetDevice = "7series"

  /** @todo Implement simulation using xsim here, if available. */

  for (sig <- Signatures) {
    /** Test simple generation of compressors */
    it should s"generate with signature $sig" in {
      getVerilogString(CompressorTree(sig, targetDevice=targetDevice))
    }
  }
}

class VersalTreeSpec extends CompressorTreeSpec {
  behavior of "Versal compressor"
  val targetDevice = "versal"

  /** @todo Implement simulation using xsim here, if available. */

  for (sig <- Signatures) {
    /** Test simple generation of compressors */
    it should s"generate with signature $sig" in {
      getVerilogString(CompressorTree(sig, targetDevice=targetDevice))
    }
  }
}

class IntelTreeSpec extends CompressorTreeSpec {
  behavior of "Intel compressor"
  val targetDevice = "intel"

  for (sig <- Signatures) {
    /** Test simple generation of compressors */
    it should s"generate with signature $sig" in {
      getVerilogString(CompressorTree(sig, targetDevice=targetDevice))
    }

    /** Test with some simple edge case inputs */
    it should s"function properly with signature $sig at edge case inputs" in {
      test(CompressorTree(sig, targetDevice=targetDevice))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        simpleTest(dut)
      }
    }

    /** Test with some random inputs */
    it should s"function properly with signature $sig given random inputs" in {
      test(CompressorTree(sig, targetDevice=targetDevice))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        randomTest(dut)
      }
    }
  }
}
