package approx.multiplication

import chisel3._
import chisel3.util.log2Up

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

import approx.multiplication.comptree.{ColumnTruncation, Miscounting, ORCompression, RowTruncation}

/** Common test patterns for exact multipliers */
trait ExactMultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
  val SimpleWidth  = 8
  val CommonWidths = List(4, 8, 16, 32)

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
    val w   = dut.io.a.getWidth
    val n   = w << 2
    val rng = new scala.util.Random(0)
    
    // Set inputs low
    pokeAndExpect(0.U, 0.U)(0.U)
    
    // Multiply by zero
    // ... on port A
    (0 until n/2).foreach { _ =>
      pokeAndExpect(BigInt(w, rng).U, 0.U)(0.U)
    }
    // ... and on port B
    (0 until n/2).foreach { _ =>
      pokeAndExpect(0.U, BigInt(w, rng).U)(0.U)
    }

    // Multiply by one
    // ... on port A
    (0 until n/2).foreach { _ =>
      val v = BigInt(w, rng).U
      pokeAndExpect(v, 1.U)(v)
    }
    // ... and on port B
    (0 until n/2).foreach { _ =>
      val v = BigInt(w, rng).U
      pokeAndExpect(1.U, v)(v)
    }

    // Generate a bunch of random products
    val aNums = Array.fill(n) { BigInt(w, rng) }
    val bNums = Array.fill(n) { BigInt(w, rng) }
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
    val w    = dut.io.a.getWidth
    val sext = (BigInt(1) <<   w  ) - 1
    val max  = (BigInt(1) << (w-1)) - 1
    val mask = (BigInt(1) << (2*w)) - 1

    // Set inputs low
    dut.io.a.poke(0.U)
    dut.io.b.poke(0.U)
    dut.clock.step()
    dut.io.p.expect(0.U)

    // Multiply by zero
    val n = w << 2
    val rng = new scala.util.Random(0)
    // ... on port A
    (0 until n/2).foreach { _ => 
      dut.io.a.poke(BigInt(w, rng).U)
      dut.clock.step()
      dut.io.p.expect(0.U)
    }
    // ... and on port B
    dut.io.a.poke(0.U)
    (0 until n/2).foreach { _ => 
      dut.io.b.poke(BigInt(w, rng).U)
      dut.clock.step()
      dut.io.p.expect(0.U)
    }
    dut.io.b.poke(0.U)

    // Multiply by one
    dut.io.b.poke(1.U)
    // ... on port A
    (0 until n/2).foreach { _ => 
      val v = BigInt(w, rng)
      dut.io.a.poke(v.U)
      dut.clock.step()
      val r = if (v > max) (sext << w) | v else v
      dut.io.p.expect((r & mask).U)
    }
    // ... and on port B
    dut.io.a.poke(1.U)
    (0 until n/2).foreach { _ => 
      val v = BigInt(w, rng)
      dut.io.b.poke(v.U)
      dut.clock.step()
      val r = if (v > max) (sext << w) | v else v
      dut.io.p.expect((r & mask).U)
    }
    dut.io.a.poke(0.U)
    dut.io.b.poke(0.U)

    // Generate a bunch of random products
    val aNums = Array.fill(n) { BigInt(w, rng) }
    val bNums = Array.fill(n) { BigInt(w, rng) }
    val prods = aNums.zip(bNums).map { case (a, b) => 
      val aSxt = if (a > max) (sext << w) | a else a
      val bSxt = if (b > max) (sext << w) | b else b
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
  val oddWidths = List(5, 13, 29)

  // Do simple and random tests
  it should "do simple multiplications" in {
    test(new Radix2Multiplier(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths ++ oddWidths) {
    it should s"generate with width=$width and ColumnTruncation(${width/2})" in {
      test(new Radix2Multiplier(width, approx=ColumnTruncation(width/2))) { dut => }
    }

    it should s"generate with width=$width and Miscounting(${width/2})" in {
      test(new Radix2Multiplier(width, approx=Miscounting(width/2))) { dut => }
    }

    it should s"generate with width=$width and ORCompression(${width/2})" in {
      test(new Radix2Multiplier(width, approx=ORCompression(width/2))) { dut => }
    }

    it should s"generate with width=$width and RowTruncation(${width/4})" in {
      test(new Radix2Multiplier(width, approx=RowTruncation(width/4))) { dut => }
    }

    it should s"do random $width-bit unsigned multiplications" in {
      test(new Radix2Multiplier(width))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed multiplications" in {
      test(new Radix2Multiplier(width, true, true))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}

class RecursiveMultiplierSpec extends ExactMultiplierSpec {
  behavior of "Recursive Multiplier"
  val oddWidths = List(5, 13, 29)

  // Do simple and random tests
  it should "do simple multiplications" in {
    test(new RecursiveMultiplier(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths ++ oddWidths) {
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
