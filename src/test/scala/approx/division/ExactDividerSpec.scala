package approx.division

import chisel3._
import chisel3.util.log2Up
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** Common test patterns for exact dividers */
trait ExactDividerSpec extends AnyFlatSpec with ChiselScalatestTester {
  val SimpleWidth  = 8
  val CommonWidths = List(4, 8, 16, 32)

  /** Poke two values and check for given values
   * 
   * @param a the divident
   * @param b the divisor
   * @param q the expected quotient
   * @param r the expected remainder
   */
  def pokeAndExpect[T <: Divider](a: UInt, b: UInt)(q: UInt, r: UInt)(implicit dut: T) = {
    // Advance time while the divider is busy
    while (dut.io.busy.peek().litToBoolean) dut.clock.step()
    // Supply the input values and advance time to let it consume them
    dut.io.a.poke(a)
    dut.io.b.poke(b)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    // Advance time while the divider is not done or the result is invalid
    while (!dut.io.done.peek().litToBoolean) dut.clock.step()
    if (b.litValue == 0) {
      // If the divisor was zero, check its flag
      dut.io.dbz.expect(true.B)
    } else {
      // Otherwise, check the results
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
    dut.io.start.poke(0.U)
    dut.io.busy.expect(false.B)
    dut.io.done.expect(false.B)
    dut.io.dbz.expect(false.B)

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
    val w   = dut.io.a.getWidth
    val n   = w << 2
    val rng = new scala.util.Random(0)

    // Set inputs low
    pokeAndExpect(0.U, 0.U)(0.U, 0.U)

    // Divide by zero
    (0 until n).foreach { _ =>
      pokeAndExpect(BigInt(w, rng).U, 0.U)(BigInt(w, rng).U, BigInt(w, rng).U)
    }

    // Divice by one
    (0 until n).foreach { _ =>
      val q = BigInt(w, rng).U
      pokeAndExpect(q, 1.U)(q, 0.U)
    }

    // Generate a bunch of random quotients
    val aNums = Array.fill(n) { BigInt(w, rng) }
    val bNums = Array.fill(n) { BigInt(w, rng) }
    val quots = aNums.zip(bNums).map { case (a, b) => if (b == 0) BigInt(0) else a / b }
    val rems  = aNums.zip(bNums).map { case (a, b) => if (b == 0) BigInt(0) else a % b }
    (0 until n).foreach { i =>
      pokeAndExpect(aNums(i).U, bNums(i).U)(quots(i).U, rems(i).U)
    }
  }
}

class Radix2DividerSpec extends ExactDividerSpec {
  behavior of "Radix 2 Divider"

  it should "do simple divisions" in {
    test(new Radix2Divider(SimpleWidth))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      simpleTest(dut)
    }
  }

  for (width <- CommonWidths) {
    it should s"do random $width-bit divisions" in {
      test(new Radix2Divider(width))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }
  }
}
