package approx.division

import chisel3._
import chisel3.util.log2Up
import chisel3.simulator.scalatest.ChiselSim

import org.scalatest.flatspec.AnyFlatSpec

/** Common test patterns for exact dividers */
trait ExactDividerSpec extends AnyFlatSpec with ChiselSim {
  val SimpleWidth = 8
  val CommonWidths = List(4, 8, 16)

  /** Poke two values and check for given values
   * 
   * @param a the divident
   * @param b the divisor
   * @param q the expected quotient
   * @param r the expected remainder
   */
  def pokeAndExpect[T <: Divider](a: UInt, b: UInt)(q: UInt, r: UInt)(implicit dut: T) = {
    dut.io.a.poke(a)
    dut.io.b.poke(b)
    dut.clock.step()
    if (b.litValue == 0) {
      dut.io.q.expect(0.U)
      dut.io.r.expect(a)
    } else {
      dut.io.q.expect(q)
      dut.io.r.expect(r)
    }
  }

  /** Simple edge case division tests
   * 
   * @param dut a divider module
   */
  def simpleTest[T <: Divider](implicit dut: T) = {
    val width = dut.io.q.getWidth
    require(width >= log2Up(42), "divider must be wide enough to take 42 as input")

    // Set inputs low
    dut.io.a.poke(0.U)
    dut.io.b.poke(0.U)
    dut.clock.step()
    dut.io.q.expect(0.U)
    dut.io.r.expect(0.U)

    // Check dividing zero by one
    pokeAndExpect(0.U, 1.U)(0.U, 0.U)

    // Check dividing zero by more than one
    pokeAndExpect(0.U, 42.U)(0.U, 0.U)

    // Check dividing something by one
    pokeAndExpect(42.U, 1.U)(42.U, 0.U)

    // Check dividing something by something without remainder
    pokeAndExpect(42.U, 14.U)(3.U, 0.U)

    // Check dividing something by something with remainder
    pokeAndExpect(17.U, 7.U)(2.U, 3.U)

    // Check dividing something by zero (quotient and remainder ignored)
    pokeAndExpect(42.U, 0.U)(0.U, 0.U)
  }

  /** Random-valued unsigned division tests
   * 
   * @param dut an unsigned divider module
   */
  def randomUnsignedTest[T <: Divider](implicit dut: T) = {
    val w = dut.io.a.getWidth
    val n = w << 2
    val rng = new scala.util.Random(0)

    // Set inputs low
    pokeAndExpect(0.U, 0.U)(0.U, 0.U)
    (0 until n).foreach { _ =>
      val aVal = BigInt(w, rng)
      pokeAndExpect(aVal.U, 0.U)(0.U, aVal.U)
    }

    // Divide by zero
    (0 until n).foreach { _ =>
      val qVal = BigInt(w, rng)
      pokeAndExpect(qVal.U, 1.U)(qVal.U, 0.U)
    }

    // Generate a bunch of random quotients
    val aNums = Array.fill(n) { BigInt(w, rng) }
    val bNums = Array.fill(n) { BigInt(w, rng) }
    val quots = aNums.zip(bNums).map { case (aVal, bVal) => if (bVal == 0) BigInt(0) else aVal / bVal }
    val rems  = aNums.zip(bNums).map { case (aVal, bVal) => if (bVal == 0) aVal else aVal % bVal }
    (0 until n).foreach { i =>
      pokeAndExpect(aNums(i).U, bNums(i).U)(quots(i).U, rems(i).U)
    }
  }
}

class Radix2DividerSpec extends ExactDividerSpec {
  behavior of "Radix 2 Combinational Divider"

  it should "do simple divisions" in {
    simulate(new Radix2Divider(SimpleWidth)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit divisions" in {
      simulate(new Radix2Divider(width)) { dut =>
        randomUnsignedTest(dut)
      }
    }
  }
}

class Radix4DividerSpec extends ExactDividerSpec {
  behavior of "Radix 4 Combinational Divider"

  it should "do simple divisions" in {
    simulate(new Radix4Divider(SimpleWidth)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit divisions" in {
      simulate(new Radix4Divider(width)) { dut =>
        randomUnsignedTest(dut)
      }
    }
  }
}
