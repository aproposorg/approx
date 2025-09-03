package approx.util

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

/** Common test patterns for LOD and LOPD modules */
trait LOSpec extends AnyFlatSpec with ChiselSim {
  val SimpleWidth  = 8
  val CommonWidths = List(4, 8, 16, 32)

  // Manually tabulated examples
  val simpleTests: Seq[(BigInt, Boolean, BigInt, BigInt)] = Seq(
    ( 0, true,   0, 0),
    ( 1, false,  1, 0),
    (42, false, 32, 5)
  )

  // Randomly generated examples
  /** Return the leading-one position of a number
   * @param v the number
   * @return the index of the leading one
   */
  def lopd(v: BigInt, ind: Int = 0): Int = if (v == 0) ind else {
    val lowest = v.lowestSetBit
    lopd(v.clearBit(lowest), lowest)
  }

  val rng = new scala.util.Random(42)
  val randomTests: (Int, Int) => Seq[(BigInt, Boolean, BigInt, BigInt)] = (w: Int, n: Int) => {
    val nums  = (0 until n).map(_ => BigInt(w, rng))
    val zeros = nums.map(_ == 0)
    val poss  = nums.map(lopd(_))
    val outs  = poss.zip(zeros).map { case (pos, zero) => if (zero) BigInt(0) else BigInt(0).setBit(pos) }
    nums.zip(zeros).zip(outs).zip(poss).map { case (((num, zero), out), pos) => (num, zero, out, BigInt(pos)) }
  }
}

class LODSpec extends LOSpec {
  behavior of "Leading-one detector"

  /** Poke a value and check for given values
   * 
   * @param in the input value
   * @param zero the expected zero flag
   * @param out the expected output value
   */
  def pokeAndExpect(dut: LOD)(in: UInt)(zero: Bool, out: UInt) = {
    dut.io.in.poke(in)
    dut.clock.step() // advance time for visual changes in VCD
    dut.io.zero.expect(zero)
    dut.io.out.expect(out)
  }

  it should "do simple detections" in {
    simulate(new LOD(SimpleWidth)) { dut =>
      simpleTests.foreach { case (in, zero, out, _) =>
        pokeAndExpect(dut)(in.U)(zero.B, out.U)
      }
    }
  }

  it should "do random detections" in {
    for (width <- CommonWidths) {
      simulate(new LOD(width)) { dut =>
        randomTests(width, 10*width).foreach { case (in, zero, out, _) =>
          pokeAndExpect(dut)(in.U)(zero.B, out.U)
        }
      }
    }
  }
}

class LOPDSpec extends LOSpec {
  behavior of "Leading-one position detector"

  /** Poke a value and check for given values
   * 
   * @param in the input value
   * @param zero the expected zero flag
   * @param out the expected output index
   */
  def pokeAndExpect(dut: LOPD)(in: UInt)(zero: Bool, out: UInt) = {
    dut.io.in.poke(in)
    dut.clock.step() // advance time for visual changes in VCD
    dut.io.zero.expect(zero)
    dut.io.out.expect(out)
  }

  it should "do simple detections" in {
    simulate(new LOPD(SimpleWidth)) { dut =>
      simpleTests.foreach { case (in, zero, _, out) =>
        pokeAndExpect(dut)(in.U)(zero.B, out.U)
      }
    }
  }

  it should "do random detections" in {
    for (width <- CommonWidths) {
      simulate(new LOPD(width)) { dut =>
        randomTests(width, 10*width).foreach { case (in, zero, _, out) =>
          pokeAndExpect(dut)(in.U)(zero.B, out.U)
        }
      }
    }
  }
}
