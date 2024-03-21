package approx.accumulation

import chisel3._

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

import approx.multiplication.comptree.Signature

/** Common test patterns for exact accumulators
 * 
 * @todo Extend all these with support for pipelining!
 */
abstract trait ExactAccumulatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  val CommonWidths = List(4, 8, 16, 32)
  val OddWidths    = List(5, 13, 29)

  /** Random-valued unsigned accumulation test */
  def randomUnsignedTest[T <: Module](implicit dut: T): Unit

  /** Random-valued signed accumulation test */
  def randomSignedTest[T <: Module](implicit dut: T): Unit
}

trait SASpec extends ExactAccumulatorSpec {
  /** Poke a value and check for a given value
   * 
   * @param in the input operand
   * @param zero whether to zero out the accumulator
   * @param acc the expected sum
   */
  def pokeAndExpect[T <: SA](in: UInt, zero: Bool)(acc: UInt)(implicit dut: T) = {
    dut.io.in.poke(in)
    dut.io.zero.poke(zero)
    dut.clock.step()
    dut.io.acc.expect(acc)
  }

  /** Poke some values and check for a given value
   * 
   * @param ins the input operands
   * @param zero whether to zero out the accumulator
   * @param acc the expected sum
   * 
   * Assigns given inputs to the least significant inputs to the module
   * and zeroes to the remaining entries, if passed fewer values than
   * inputs. Ignores extra inputs in the opposite case.
   */
  def pokeAndExpect[T <: PSA](ins: Seq[UInt], zero: Bool)(acc: UInt)(implicit dut: T) = {
    val insExt = if (ins.size < dut.io.ins.size) ins ++ Seq.fill(dut.io.ins.size - ins.size)(0.U) else ins
    dut.io.ins.zip(insExt).foreach { case (port, inv) => port.poke(inv) }
    dut.io.zero.poke(zero)
    dut.clock.step()
    dut.io.acc.expect(acc)
  }

  /** Random-valued unsigned accumulation test
   * 
   * @param dut an unsigned simple accumulator module
   */
  def randomUnsignedTest[T <: Module](implicit dut: T) = {
    val rng = new scala.util.Random(42)
    var acc = BigInt(0)

    dut match {
      case sa: SA =>
        val mask = (BigInt(1) << sa.accW) - 1
        val n = sa.accW << 2

        // Set inputs low
        pokeAndExpect(0.U, false.B)(0.U)(sa)

        // Accumulate some numbers
        val ins   = Array.fill(n) { BigInt(sa.inW, rng) }
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          acc = if (zeros(i)) ins(i) else (acc + ins(i)) & mask
          pokeAndExpect(ins(i).U, zeros(i).B)(acc.U)(sa)
        }

      case psa: PSA =>
        val mask = (BigInt(1) << psa.accW) - 1
        val n = psa.accW << 2

        // Set inputs low
        pokeAndExpect(Seq.fill(psa.nIn)(0.U), false.B)(0.U)(psa)

        // Accumulate some numbers
        val ins   = Array.fill(n) { Seq.fill(psa.nIn) { BigInt(psa.inW, rng) } }
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          acc = if (zeros(i)) ins(i).sum & mask else (acc + ins(i).sum) & mask
          pokeAndExpect(ins(i).map(_.U), zeros(i).B)(acc.U)(psa)
        }

      case _ => throw new IllegalArgumentException("can only verify SAs and PSAs")
    }
  }

  /** Random-valued signed accumulation test
   * 
   * @param dut a signed simple accumulator module
   */
  def randomSignedTest[T <: Module](implicit dut: T) = {
    val rng = new scala.util.Random(42)
    var acc = BigInt(0)

    dut match {
      case sa: SA =>
        val mask = (BigInt(1) << sa.accW) - 1
        val n = sa.accW << 2

        // Set inputs low
        pokeAndExpect(0.U, false.B)(0.U)(sa)

        // Accumulate some numbers
        val ext = ((BigInt(1) << sa.accW) - 1) & ~((BigInt(1) << sa.inW) - 1)
        val ins   = Array.fill(n) { BigInt(sa.inW, rng) }
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          val inExt = if (ins(i).testBit(sa.inW-1)) ext | ins(i) else ins(i)
          acc = if (zeros(i)) inExt else (acc + inExt) & mask
          pokeAndExpect(ins(i).U, zeros(i).B)(acc.U)(sa)
        }

      case psa: PSA =>
        val mask = (BigInt(1) << psa.accW) - 1
        val n = psa.accW << 2

        // Set inputs low
        pokeAndExpect(Seq.fill(psa.nIn)(0.U), false.B)(0.U)(psa)

        // Accumulate some numbers
        val ext = ((BigInt(1) << psa.accW) - 1) & ~((BigInt(1) << psa.inW) - 1)
        val ins   = Array.fill(n) { Seq.fill(psa.nIn) { BigInt(psa.inW, rng) }}
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          val inExt = ins(i).map { in => if (in.testBit(psa.inW-1)) ext | in else in }
          acc = if (zeros(i)) inExt.sum & mask else (acc + inExt.sum) & mask
          pokeAndExpect(ins(i).map(_.U), zeros(i).B)(acc.U)(psa)
        }

      case _ => throw new IllegalArgumentException("can only verify SAs and PSAs")
    }
  }
}

class SimpleAccumulatorSpec extends SASpec {
  behavior of "Simple Accumulator"

  for (width <- CommonWidths ++ OddWidths) {
    it should s"do random $width-bit unsigned accumulation" in {
      test(new SimpleAccumulator(width-3, width))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed accumulation" in {
      test(new SimpleAccumulator(width-3, width, true))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}

class ParallelSimpleAccumulatorSpec extends SASpec {
  behavior of "Parallel Simple Accumulator"

  for (width <- CommonWidths ++ OddWidths) {
    it should s"do random $width-bit unsigned accumulation" in {
      test(new ParallelSimpleAccumulator(width / 2, width-3, width))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed accumulation" in {
      test(new ParallelSimpleAccumulator(width / 2, width-3, width, true))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}

trait MACSpec extends ExactAccumulatorSpec {
  /** Poke a pair of values and check for a given value
   * 
   * @param a the first operand
   * @param b the second operand
   * @param zero whether to zero out the accumulator
   * @param acc the expected sum
   */
  def pokeAndExpect[T <: MAC](a: UInt, b: UInt, zero: Bool)(acc: UInt)(implicit dut: T) = {
    dut.io.a.poke(a)
    dut.io.b.poke(b)
    dut.io.zero.poke(zero)
    dut.clock.step()
    dut.io.acc.expect(acc)
  }

  /** Poke some values and check for a given value
   * 
   * @param as the first operands
   * @param bs the second operands
   * @param zero whether to zero out the accumulator
   * @param acc the expected sum
   * 
   * Assigns given inputs to the least significant inputs to the module
   * and zeroes to the remaining entries, if passed fewer values than
   * inputs. Ignores extra inputs in the opposite case.
   */
  def pokeAndExpect[T <: PMAC](as: Seq[UInt], bs: Seq[UInt], zero: Bool)(acc: UInt)(implicit dut: T) = {
    if (as.size != bs.size)
      println(s"Warning: ignoring values on MAC input")
    dut.io.as.zip(as).foreach { case (port, inv) => port.poke(inv) }
    dut.io.bs.zip(bs).foreach { case (port, inv) => port.poke(inv) }
    dut.io.zero.poke(zero)
    dut.clock.step()
    dut.io.acc.expect(acc)
  }

  /** Random-valued unsigned accumulation test
   * 
   * @param dut an unsigned simple accumulator module
   */
  def randomUnsignedTest[T <: Module](implicit dut: T) = {
    val rng = new scala.util.Random(42)
    var acc = BigInt(0)

    dut match {
      case mac: MAC =>
        val mask = (BigInt(1) << mac.accW) - 1
        val n = mac.accW << 2

        // Set inputs low
        pokeAndExpect(0.U, 0.U, false.B)(0.U)(mac)

        // Accumulate some numbers
        val as    = Array.fill(n) { BigInt(mac.inW, rng) }
        val bs    = Array.fill(n) { BigInt(mac.inW, rng) }
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          acc = if (zeros(i)) (as(i) * bs(i)) & mask else (acc + as(i) * bs(i)) & mask
          pokeAndExpect(as(i).U, bs(i).U, zeros(i).B)(acc.U)(mac)
        }

      case pmac: PMAC =>
        val mask = (BigInt(1) << pmac.accW) - 1
        val n = pmac.accW << 2

        // Set inputs low
        pokeAndExpect(Seq.fill(pmac.nIn)(0.U), Seq.fill(pmac.nIn)(0.U), false.B)(0.U)(pmac)

        // Accumulate some numbers
        val as    = Array.fill(n) { Seq.fill(pmac.nIn) { BigInt(pmac.inW, rng) } }
        val bs    = Array.fill(n) { Seq.fill(pmac.nIn) { BigInt(pmac.inW, rng) } }
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          acc = if (zeros(i)) as(i).zip(bs(i)).map{ case (a, b) => a * b }.sum & mask
          else (acc + as(i).zip(bs(i)).map{ case (a, b) => a * b }.sum) & mask
          pokeAndExpect(as(i).map(_.U), bs(i).map(_.U), zeros(i).B)(acc.U)(pmac)
        }

      case _ => throw new IllegalArgumentException("can only verify SAs and PSAs")
    }
  }

  /** Random-valued signed accumulation test
   * 
   * @param dut a signed simple accumulator module
   */
  def randomSignedTest[T <: Module](implicit dut: T) = {
    val rng = new scala.util.Random(42)
    var acc = BigInt(0)

    dut match {
      case mac: MAC =>
        val mask = (BigInt(1) << mac.accW) - 1
        val n = mac.accW << 2

        // Set inputs low
        pokeAndExpect(0.U, 0.U, false.B)(0.U)(mac)

        // Accumulate some numbers
        val ext = ((BigInt(1) << mac.accW) - 1) & ~((BigInt(1) << mac.inW) - 1)
        val as    = Array.fill(n) { BigInt(mac.inW, rng) }
        val bs    = Array.fill(n) { BigInt(mac.inW, rng) }
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          val aExt = if (as(i).testBit(mac.inW-1)) ext | as(i) else as(i)
          val bExt = if (bs(i).testBit(mac.inW-1)) ext | bs(i) else bs(i)
          acc = if (zeros(i)) (aExt * bExt) & mask else (acc + aExt * bExt) & mask
          pokeAndExpect(as(i).U, bs(i).U, zeros(i).B)(acc.U)(mac)
        }

      case pmac: PMAC =>
        val mask = (BigInt(1) << pmac.accW) - 1
        val n = pmac.accW << 2

        // Set inputs low
        pokeAndExpect(Seq.fill(pmac.nIn)(0.U), Seq.fill(pmac.nIn)(0.U), false.B)(0.U)(pmac)

        // Accumulate some numbers
        val ext = ((BigInt(1) << pmac.accW) - 1) & ~((BigInt(1) << pmac.inW) - 1)
        val as    = Array.fill(n) { Seq.fill(pmac.nIn) { BigInt(pmac.inW, rng) }}
        val bs    = Array.fill(n) { Seq.fill(pmac.nIn) { BigInt(pmac.inW, rng) }}
        val zeros = Array.fill(n) { rng.nextBoolean() }
        (0 until n).foreach { i =>
          val asExt = as(i).map { a => if (a.testBit(pmac.inW-1)) ext | a else a }
          val bsExt = bs(i).map { b => if (b.testBit(pmac.inW-1)) ext | b else b }
          acc = if (zeros(i)) asExt.zip(bsExt).map{ case (a, b) => a * b }.sum & mask
          else (acc + asExt.zip(bsExt).map{ case (a, b) => a * b }.sum) & mask
          pokeAndExpect(as(i).map(_.U), bs(i).map(_.U), zeros(i).B)(acc.U)(pmac)
        }

      case _ => throw new IllegalArgumentException("can only verify SAs and PSAs")
    }
  }
}

class MultiplyAccumulatorSpec extends MACSpec {
  behavior of "Multiply Accumulator"

  for (width <- CommonWidths ++ OddWidths) {
    it should s"do random $width-bit unsigned accumulation" in {
      test(new MultiplyAccumulator(width-3, width)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed accumulation" in {
      test(new MultiplyAccumulator(width-3, width, true)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}

class ParallelMultiplyAccumulatorSpec extends MACSpec {
  behavior of "Parallel Multiply Accumulator"

  // These tests are only run for relatively low bit-widths due to 
  // long execution times otherwise
  for (width <- CommonWidths ++ OddWidths) {
    it should s"do random $width-bit unsigned accumulation" in {
      test(new ParallelMultiplyAccumulator(width / 2, width-3, width))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        randomUnsignedTest(dut)
      }
    }

    it should s"do random $width-bit signed accumulation" in {
      test(new ParallelMultiplyAccumulator(width / 2, width-3, width, true))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        randomSignedTest(dut)
      }
    }
  }
}

trait MxACSpec extends ExactAccumulatorSpec {
  /** Poke a value and check for a given value
   * 
   * @param in the input operand
   * @param zero whether to zero out the accumulator
   * @param acc the expected sum
   */
  def pokeAndExpect[T <: MxAC](in: UInt, zero: Bool)(acc: UInt)(implicit dut: T) = {
    dut.io.in.poke(in)
    dut.io.zero.poke(zero)
    dut.clock.step()
    dut.io.acc.expect(acc)
  }

  /** Matrices are by default sign-free */
  def randomUnsignedTest[T <: Module](implicit dut: T) = {
    assume(false, "matrix accumulators are by default sign-free")
  }
  def randomSignedTest[T <: Module](implicit dut: T) = {
    assume(false, "matrix accumulators are by default sign-free")
  }

  /** Random-valued accumulation test
   * 
   * @param dut a matrix accumulator module
   */
  def randomTest[T <: MxAC](implicit dut: T): Unit = {
    val rng  = new scala.util.Random(42)
    val mask = (BigInt(1) << dut.accW) - 1
    var acc  = BigInt(0)
    val n    = dut.accW << 2

    /** Sum the elements of a bit matrix
     * 
     * @param sig the signature of the bit matrix
     * @param bits the flattened string of input bits
     */
    def bitMatrixSum(sig: Signature, bits: BigInt): BigInt = {
      var sum = BigInt(0)
      var offset = 0
      sig.signature.zipWithIndex.foreach { case (cnt, wght) =>
        (0 until cnt).foreach { _ =>
          if (bits.testBit(offset)) sum += BigInt(1) << wght
          offset += 1
        }
      }
      sum
    }

    // Set the inputs low
    pokeAndExpect(0.U, false.B)(0.U)(dut)

    // Accumulate some numbers
    val ins   = Array.fill(n) { BigInt(dut.sig.count, rng) }
    val zeros = Array.fill(n) { rng.nextBoolean() }
    (0 until n).foreach { i =>
      acc = if (zeros(i)) bitMatrixSum(dut.sig, ins(i)) & mask else (acc + bitMatrixSum(dut.sig, ins(i))) & mask
      pokeAndExpect(ins(i).U, zeros(i).B)(acc.U)(dut)
    }
  }
}

class BitMatrixAccumulatorSpec extends MxACSpec {
  behavior of "Bit Matrix Accumulator"
  implicit val rng = new scala.util.Random(0)

  for (width <- CommonWidths ++ OddWidths) {
    it should s"do random $width-bit accumulation" in {
      val sig = new Signature(Array.fill(width)(rng.nextInt(2 * width)))
      test(new BitMatrixAccumulator(sig, width))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        randomTest(dut)
      }
    }
  }
}
