package approx.addition

import chisel3._
import chisel3.util.log2Up

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

/** Common test patterns for exact, self-timed adders */
trait ExactSelfTimedAdderSpec extends AnyFlatSpec with ChiselScalatestTester {
  val SimpleWidth  = 8
  val CommonWidths = List(4, 8, 16, 32)
  
  /** Poke three given values and check for given values
   * 
   * @param a the first operand
   * @param b the second operand
   * @param cin the carry-in
   * @param s the expected sum
   * @param f the expected finish
   * @param cout the expected carry-out
   */
  def pokeAndExpect[T <: SelfTimedAdder](a: UInt, b: UInt, cin: Bool)(s: UInt, f: Bool, cout: Bool)(implicit dut: T) = {
    dut.io.a.poke(a)
    dut.io.b.poke(b)
    dut.io.cin.poke(cin)
    dut.clock.step() // advance time for visual changes in VCD
    dut.io.s.expect(s)
    dut.io.f.expect(f)
    dut.io.cout.expect(cout)
  }

  /** Simple edge case addition tests
   * 
   * @param dut an adder module (width must be wide enough to support a sum of 42)
   */
  def simpleTest[T <: SelfTimedAdder](implicit dut: T) = {
    val width = dut.io.s.getWidth
    require(width >= log2Up(42), "adder must be wide enough to calculate a sum of 42")

    val min  =  BigInt(1) << (width-1)
    val max  = (BigInt(1) << (width-1)) - 1
    val neg1 = (BigInt(1) <<  width) - 1

    // Set inputs low
    pokeAndExpect(0.U, 0.U, false.B)(0.U, true.B, false.B)

    // Check addition to zero
    // ... on port A
    pokeAndExpect(0.U, 41.U, true.B)(42.U, true.B, false.B)
    
    // ... on port B
    pokeAndExpect(42.U, 0.U, false.B)(42.U, true.B, false.B)
    
    // Check addition to one
    // ... on port A
    pokeAndExpect(41.U, 1.U, false.B)(42.U, true.B, false.B)
    
    // ... on port B
    pokeAndExpect(1.U, 40.U, true.B)(42.U, true.B, false.B)
    
    // Check overflow
    pokeAndExpect(1.U, max.U, false.B)(min.U, true.B, false.B)
    
    // Check underflow
    pokeAndExpect(min.U, neg1.U, false.B)(max.U, true.B, true.B)
  }

  /** Random-valued addition tests
   * 
   * @param dut an adder module
   */
  def randomTest[T <: SelfTimedAdder](implicit dut: T) = {
    val width = dut.io.s.getWidth
    val n     = width << 2
    val rng   = new scala.util.Random(0)
    val mask  = (BigInt(1) << width) - 1

    // Generate a bunch of random sums
    val aNums = Array.fill(n) { BigInt(width, rng) }
    val bNums = Array.fill(n) { BigInt(width, rng) }
    val cins  = Array.fill(n) { rng.nextBoolean() }
    val res   = (0 until n).map { i =>
      val sum = aNums(i) + bNums(i) + (if (cins(i)) 1 else 0)
      (sum >> width, sum & mask)
    }
    (0 until n).foreach { i =>
      pokeAndExpect(aNums(i).U, bNums(i).U, cins(i).B)(res(i)._2.U, true.B, res(i)._1.B)
    }
  }
}

class STASpec extends ExactSelfTimedAdderSpec {
  behavior of "Self-Timed Adder"

  it should "do simple additions" in {
    test(new STA(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit additions" in {
      test(new STA(width))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        randomTest(dut)
      }
    }
  }
}

class CCASpec extends ExactSelfTimedAdderSpec {
  behavior of "Carry-Skip Adder"

  it should "do simple additions" in {
    test(new CCA(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit additions" in {
      test(new CCA(width))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        randomTest(dut)
      }
    }
  }
}
