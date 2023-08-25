package approx.multiplication

import chisel3._
import chisel3.util.log2Up

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

/** Common test patterns for exact sequential multipliers */
trait ExactSeqMultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
  val SimpleWidth  = 8
  val CommonWidths = List(4, 8, 16, 32)

  /** Poke two given values and wait for a handshake
   * 
   * @param a the first operand
   * @param b the second operand
   */
  def pokeAndHandshake[T <: SeqMultiplier](a: UInt, b: UInt)(implicit dut: T) = {
    dut.io.in.valid.poke(true.B)
    dut.io.in.bits.a.poke(a)
    dut.io.in.bits.b.poke(b)
    while (!dut.io.in.ready.peek().litToBoolean) {
      dut.clock.step() // wait for ready
    }
    dut.clock.step() // perform handshake
    dut.io.in.valid.poke(false.B)
  }

  /** Wait for a valid output and check for a given value
   * 
   * @param prod the expected product
   */
  def waitAndExpect[T <: SeqMultiplier](prod: UInt)(implicit dut: T) = {
    dut.io.out.ready.poke(true.B)
    while (!dut.io.out.valid.peek().litToBoolean) {
      dut.clock.step() // wait for valid
    }
    dut.io.out.bits.p.expect(prod)
    dut.clock.step() // perform handshake
    dut.io.out.ready.poke(false.B)
  }

  /** Simple edge case multiplication tests
   * 
   * @param dut a sequential multiplier module (width must be wide enough to support a product of 42)
   */
  def simpleTest[T <: SeqMultiplier](implicit dut: T) = {
    require(dut.io.out.bits.p.getWidth >= log2Up(42), "multiplier must be wide enough to calculate a product of 42")

    // Set inputs and control signals low
    dut.io.in.valid.poke(false.B)
    dut.io.in.bits.a.poke(0.U)
    dut.io.in.bits.b.poke(0.U)
    dut.io.out.ready.poke(false.B)
    dut.clock.step()
    dut.io.in.ready.expect(true.B)
    dut.io.out.valid.expect(false.B)

    // Simple multiplication tests
    pokeAndHandshake(1.U, 0.U)
    waitAndExpect(0.U)

    pokeAndHandshake(0.U, 1.U)
    waitAndExpect(0.U)

    pokeAndHandshake(1.U, 1.U)
    waitAndExpect(1.U)

    // Another simple example
    pokeAndHandshake(14.U, 3.U)
    waitAndExpect(42.U)
  }

  /** Random-valued unsigned multiplication tests
   * 
   * @param dut an unsigned sequential multiplier module
   */
  def randomUnsignedTest[T <: SeqMultiplier](implicit dut: T) = {
    val w   = dut.io.in.bits.a.getWidth
    val n   = w << 2
    val rng = new scala.util.Random(0)

    // Set inputs and control signals low
    dut.io.in.valid.poke(false.B)
    dut.io.in.bits.a.poke(0.U)
    dut.io.in.bits.b.poke(0.U)
    dut.io.out.ready.poke(false.B)
    dut.clock.step()
    dut.io.in.ready.expect(true.B)
    dut.io.out.valid.expect(false.B)

    // Multiply by zero
    // ... on port A
    (0 until n/2).foreach { _ =>
      pokeAndHandshake(BigInt(w, rng).U, 0.U)
      waitAndExpect(0.U)
    }
    // ... and on port B
    (0 until n/2).foreach { _ =>
      pokeAndHandshake(0.U, BigInt(w, rng).U)
      waitAndExpect(0.U)
    }

    // Multiply by one
    // ... on port A
    (0 until n/2).foreach { _ =>
      val v = BigInt(w, rng).U
      pokeAndHandshake(v, 1.U)
      waitAndExpect(v)
    }
    // ... and on port B
    (0 until n/2).foreach { _ =>
      val v = BigInt(w, rng).U
      pokeAndHandshake(1.U, v)
      waitAndExpect(v)
    }

    // Generate a bunch of random products
    val aNums = Array.fill(n) { BigInt(w, rng) }
    val bNums = Array.fill(n) { BigInt(w, rng) }
    val prods = aNums.zip(bNums).map { case (a, b) => a * b }
    (0 until n).foreach { i =>
      pokeAndHandshake(aNums(i).U, bNums(i).U)
      waitAndExpect(prods(i).U)
    }
  }
}

class Radix2SeqMultiplierSpec extends ExactSeqMultiplierSpec {
  behavior of "Radix-2 Sequential Multiplier"

  // Do simple and random unsigned tests
  it should "do simple multiplications" in {
    test(new Radix2SeqMultiplier(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit unsigned multiplications" in {
      test(new Radix2SeqMultiplier(width))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }
  }
}
