package approx.multiplication

import chisel3._
import chisel3.util.log2Up

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

import approx.multiplication.comptree.{Approximation, ColumnTruncation, Miscounting, ORCompression, RowTruncation}

/** Common test patterns for exact multipliers */
trait ExactMultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
  val SimpleWidth  = 8
  val CommonWidths = List(4, 8, 16, 32)
  val OddWidths    = List(5, 13, 29)

  /** Poke two values and check for a given value
   * 
   * @param a the first operand
   * @param b the second operand
   * @param p the expected product
   */
  def pokeAndExpect[T <: Multiplier](a: UInt, b: UInt)(p: UInt)(implicit dut: T) = {
    dut.io.a.poke(a)
    dut.io.b.poke(b)
    dut.clock.step() // advance time for visual changes in VCD
    dut.io.p.expect(p)
  }

  /** Simple edge case multiplication tests
   * 
   * @param dut a multiplier module (width must be wide enough to support a product of 42)
   */
  def simpleTest[T <: Multiplier](implicit dut: T) = {
    require(dut.io.p.getWidth >= log2Up(42), "multiplier must be wide enough to calculate a product of 42")

    // Set inputs low
    pokeAndExpect(0.U, 0.U)(0.U)
    
    // Simple multiplication tests
    pokeAndExpect(1.U, 0.U)(0.U)
    
    pokeAndExpect(0.U, 1.U)(0.U)
    
    pokeAndExpect(1.U, 1.U)(1.U)
    
    // Another simple example
    pokeAndExpect(14.U, 3.U)(42.U)
  }

  /** Random-valued unsigned multiplication tests
   * 
   * @param dut an unsigned multiplier module
   */
  def randomUnsignedTest[T <: Multiplier](implicit dut: T) = {
    val (aW, bW) = (dut.aWidth, dut.bWidth)
    val n   = scala.math.max(aW, bW) << 2
    val rng = new scala.util.Random(0)
    
    // Set inputs low
    pokeAndExpect(0.U, 0.U)(0.U)
    
    // Multiply by zero
    // ... on port A
    (0 until n/2).foreach { _ =>
      pokeAndExpect(BigInt(aW, rng).U, 0.U)(0.U)
    }
    // ... and on port B
    (0 until n/2).foreach { _ =>
      pokeAndExpect(0.U, BigInt(bW, rng).U)(0.U)
    }

    // Multiply by one
    // ... on port A
    (0 until n/2).foreach { _ =>
      val v = BigInt(aW, rng).U
      pokeAndExpect(v, 1.U)(v)
    }
    // ... and on port B
    (0 until n/2).foreach { _ =>
      val v = BigInt(bW, rng).U
      pokeAndExpect(1.U, v)(v)
    }

    // Generate a bunch of random products
    val aNums = Array.fill(n) { BigInt(aW, rng) }
    val bNums = Array.fill(n) { BigInt(bW, rng) }
    val prods = aNums.zip(bNums).map { case (a, b) => a * b }
    (0 until n).foreach { i =>
      pokeAndExpect(aNums(i).U, bNums(i).U)(prods(i).U)
    }
  }

  /** Random-valued signed multiplication tests
   * 
   * @param dut a signed multiplier module
   */
  def randomSignedTest[T <: Multiplier](dut: T) = {
    val (aW,    bW)    = (dut.aWidth, dut.bWidth)
    val (aSext, bSext) = ((BigInt(1) <<   bW  ) - 1, (BigInt(1) <<   aW  ) - 1)
    val (aMax,  bMax)  = ((BigInt(1) << (aW-1)) - 1, (BigInt(1) << (bW-1)) - 1)
    val mask = (BigInt(1) << (aW + bW)) - 1

    // Set inputs low
    dut.io.a.poke(0.U)
    dut.io.b.poke(0.U)
    dut.clock.step()
    dut.io.p.expect(0.U)

    // Multiply by zero
    val n = scala.math.max(aW, bW) << 2
    val rng = new scala.util.Random(0)
    // ... on port A
    (0 until n/2).foreach { _ => 
      dut.io.a.poke(BigInt(aW, rng).U)
      dut.clock.step()
      dut.io.p.expect(0.U)
    }
    // ... and on port B
    dut.io.a.poke(0.U)
    (0 until n/2).foreach { _ => 
      dut.io.b.poke(BigInt(bW, rng).U)
      dut.clock.step()
      dut.io.p.expect(0.U)
    }
    dut.io.b.poke(0.U)

    // Multiply by one
    dut.io.b.poke(1.U)
    // ... on port A
    (0 until n/2).foreach { _ => 
      val v = BigInt(aW, rng)
      dut.io.a.poke(v.U)
      dut.clock.step()
      val r = if (v > aMax) (aSext << aW) | v else v
      dut.io.p.expect((r & mask).U)
    }
    // ... and on port B
    dut.io.a.poke(1.U)
    (0 until n/2).foreach { _ => 
      val v = BigInt(bW, rng)
      dut.io.b.poke(v.U)
      dut.clock.step()
      val r = if (v > bMax) (bSext << bW) | v else v
      dut.io.p.expect((r & mask).U)
    }
    dut.io.a.poke(0.U)
    dut.io.b.poke(0.U)

    // Generate a bunch of random products
    val aNums = Array.fill(n) { BigInt(aW, rng) }
    val bNums = Array.fill(n) { BigInt(bW, rng) }
    val prods = aNums.zip(bNums).map { case (a, b) => 
      val aSxt = if (a > aMax) (aSext << aW) | a else a
      val bSxt = if (b > bMax) (bSext << bW) | b else b
      (aSxt * bSxt) & mask
    }
    (0 until n).foreach { i =>
      dut.io.a.poke(aNums(i).U)
      dut.io.b.poke(bNums(i).U)
      dut.clock.step()
      dut.io.p.expect(prods(i).U)
    }
  }
}

class Radix2MultiplierSpec extends ExactMultiplierSpec {
  behavior of "Radix-2 Multiplier"

  /** Run a generation test */
  def generationTest(aWidth: Int, bWidth: Int, aSigned: Boolean, bSigned: Boolean, approx: Seq[Approximation]) = {
    it should s"generate signed=${aSigned || bSigned} with aWidth=$aWidth and bWidth=$bWidth and approx=${approx.mkString("[", ", ", "]")}" in {
      getVerilogString(new Radix2Multiplier(aWidth, bWidth, aSigned, bSigned, comp=true, approx=approx))
    }
  }

  /** Run a combination of tests for different widths of the first operand */
  def allTests(widths: List[Int]) = {
    for (width <- widths) {
      val approxes = List(ColumnTruncation(width/2), Miscounting(width/2), ORCompression(width/2), RowTruncation(width/4))

      // Equal widths
      for (approx <- approxes; signed <- List(false, true)) {
        generationTest(width, width, signed, signed, Seq(approx))
      }

      it should s"do random $width-bit unsigned multiplication" in {
        test(new Radix2Multiplier(width, width))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomUnsignedTest(dut)
        }
      }

      it should s"do random $width-bit signed multiplication" in {
        test(new Radix2Multiplier(width, width, true, true))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomSignedTest(dut)
        }
      }

      // Non-equal widths
      for (approx <- approxes; signed <- List(false, true)) {
        generationTest(width, if (width == SimpleWidth) SimpleWidth+1 else SimpleWidth, signed, signed, Seq(approx))
      }

      it should s"do random $width by $SimpleWidth-bit unsigned multiplications" in {
        test(new Radix2Multiplier(width, SimpleWidth))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomUnsignedTest(dut)
        }
      }

      it should s"do random $width by $SimpleWidth-bit signed multiplications" in {
        test(new Radix2Multiplier(width, SimpleWidth, true, true))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomSignedTest(dut)
        }
      }
    }
  }
}

class Radix2MultiplierSpecCommon extends Radix2MultiplierSpec {
  // Do simple and random tests
  it should "do simple multiplications" in {
    test(new Radix2Multiplier(SimpleWidth, SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  // Do generation and random tests
  allTests(CommonWidths)
}

class Radix2MultiplierSpecOdd extends Radix2MultiplierSpec {
  // Do generation and random tests
  allTests(OddWidths)
}

class AdaptiveRadix2MultiplierSpec extends ExactMultiplierSpec {
  behavior of "Adaptive Radix-2 Multiplier"

  class Wrapper(aWidth: Int, bWidth: Int, approxWidth: Int,
                aSigned: Boolean, bSigned: Boolean, numModes: Int)
    extends Multiplier(aWidth, bWidth) {
    val aR2mult = Module(
      new AdaptiveRadix2Multiplier(aWidth, bWidth, approxWidth, aSigned, bSigned, numModes))
    aR2mult.io.ctrl := 0.U
    aR2mult.io.a := io.a
    aR2mult.io.b := io.b
    io.p := aR2mult.io.p
  }

  def getRandNumModes(width: Int) = (new scala.util.Random(width)).nextInt(3) + 1

  /** Run a combination of tests for different widths of the first operand */
  def allTests(widths: List[Int]) = {
    for (width <- widths) {
      // Equal widths
      it should s"do random $width-bit unsigned multiplication" in {
        test(new Wrapper(width, width, width / 2, false, false, getRandNumModes(width)))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomUnsignedTest(dut)
        }
      }

      it should s"do random $width-bit signed multiplication" in {
        test(new Wrapper(width, width, width / 2, true, true, getRandNumModes(width)))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomSignedTest(dut)
        }
      }

      // Non-equal widths
      it should s"do random $width by $SimpleWidth-bit unsigned multiplications" in {
        test(new Wrapper(width, SimpleWidth, width / 2, false, false, getRandNumModes(width - 1)))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomUnsignedTest(dut)
        }
      }

      it should s"do random $width by $SimpleWidth-bit signed multiplications" in {
        test(new Wrapper(width, SimpleWidth, width / 2, true, true, getRandNumModes(width + 1)))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomSignedTest(dut)
        }
      }
    }
  }
}

class AdaptiveRadix2MultiplierSpecCommon extends AdaptiveRadix2MultiplierSpec {
  // Do random tests
  allTests(CommonWidths)
}

class AdaptiveRadix2MultiplierSpecOdd extends AdaptiveRadix2MultiplierSpec {
  // Do random tests
  allTests(OddWidths)
}

class Radix4MultiplierSpec extends ExactMultiplierSpec {
  behavior of "Radix-4 Multiplier"

  /** Run a generation test */
  def generationTest(aWidth: Int, bWidth: Int, aSigned: Boolean, bSigned: Boolean, approx: Seq[Approximation]) = {
    it should s"generate signed=${aSigned || bSigned} with aWidth=$aWidth and bWidth=$bWidth and approx=${approx.mkString("[", ", ", "]")}" in {
      getVerilogString(new Radix4Multiplier(aWidth, bWidth, aSigned, bSigned, comp=true, approx=approx))
    }
  }

  /** Run a combination of tests for different widths of the first operand */
  def allTests(widths: List[Int]) = {
    for (width <- widths) {
      val approxes = List(ColumnTruncation(width/2), Miscounting(width/2), ORCompression(width/2), RowTruncation(width/4))

      // Equal widths
      for (approx <- approxes; signed <- List(false, true)) {
        generationTest(width, width, signed, signed, Seq(approx))
      }

      it should s"do random $width-bit unsigned multiplication" in {
        test(new Radix4Multiplier(width, width))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomUnsignedTest(dut)
        }
      }

      it should s"do random $width-bit signed multiplication" in {
        test(new Radix4Multiplier(width, width, true, true))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomSignedTest(dut)
        }
      }

      // Non-equal widths
      for (approx <- approxes; signed <- List(false, true)) {
        generationTest(width, if (width == SimpleWidth) SimpleWidth+1 else SimpleWidth, signed, signed, Seq(approx))
      }

      it should s"do random $width by $SimpleWidth-bit unsigned multiplications" in {
        test(new Radix4Multiplier(width, SimpleWidth))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomUnsignedTest(dut)
        }
      }

      it should s"do random $width by $SimpleWidth-bit signed multiplications" in {
        test(new Radix4Multiplier(width, SimpleWidth, true, true))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
          randomSignedTest(dut)
        }
      }
    }
  }
}

class Radix4MultiplierSpecCommon extends Radix4MultiplierSpec {
  // Do simple and random tests
  it should "do simple multiplications" in {
    test(new Radix4Multiplier(SimpleWidth, SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  // Do generation and random tests
  allTests(CommonWidths)
}

class Radix4MultiplierSpecOdd extends Radix4MultiplierSpec {
  // Do generation and random tests
  allTests(OddWidths)
}

class RecursiveMultiplierSpec extends ExactMultiplierSpec {
  behavior of "Recursive Multiplier"

  // Do simple and random tests
  it should "do simple multiplications" in {
    test(new RecursiveMultiplier(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths ++ OddWidths) {
    it should s"generate with width=$width inexact 2x2 multipliers in the ${width/2} LSBs" in {
      test(new RecursiveMultiplier(width, width/2)) { dut => }
    }

    it should s"do random $width-bit unsigned multiplications" in {
      test(new RecursiveMultiplier(width))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed multiplications" in {
      test(new RecursiveMultiplier(width, signed=true))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}

class AlphabetSetMultiplierSpec extends ExactMultiplierSpec {
  behavior of "Alphabet Set Multiplier"

  // Do simple and random tests
  it should "do simple multiplications" in {
    test(new AlphabetSetMultiplier(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit unsigned multiplications" in {
      test(new AlphabetSetMultiplier(width))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed multiplications" in {
      test(new AlphabetSetMultiplier(width, true))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}
