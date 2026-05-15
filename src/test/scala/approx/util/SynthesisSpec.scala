package approx.util

import java.nio.file.{Files, Paths}

import scala.util.{Try, Success, Failure}

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class SynthesisSpec extends AnyFlatSpec with Matchers {
  behavior of "Synthesis"

  val runVivado = System.getenv().containsKey("XILINX_VIVADO")

  if (runVivado) {
    val vivadoPath = System.getenv("XILINX_VIVADO")
    println(s"Vivado environment detected at ${vivadoPath}")

    it should "generate Vivado synthesis and implementation sources for an RCA adder" in {
      val (dir, sv) = Synthesis.generateVivadoSources(() => new approx.addition.RCA(8)) match {
        case Success(result)  => result
        case Failure(exp)     => fail(s"Source generation failed with exception: ${exp.getMessage}")
      }
      val svPath = Paths.get(dir, sv)
      Files.exists(svPath) shouldBe true
    }

    it should "run Vivado synthesis and parse results for an RCA adder" in {
      val (dir, sv) = Synthesis.generateVivadoSources(() => new approx.addition.RCA(8)) match {
        case Success(result)  => result
        case Failure(exp)     => fail(s"Source generation failed with exception: ${exp.getMessage}")
      }
      val results = Synthesis.runVivadoSynthesis(dir)
      println(s"Synthesis results: ${results}")
      results.report shouldBe defined
      results.lut    shouldBe defined
      results.lut    should equal(Some(8))
      results.ff     shouldBe defined
      results.ff     should equal(Some(0))
      results.dsp    shouldBe defined
      results.dsp    should equal(Some(0))
    }

    it should "run Vivado implementation and parse results for an RCA adder" in {
      val (dir, sv) = Synthesis.generateVivadoSources(() => new approx.addition.RCA(8)) match {
        case Success(result)  => result
        case Failure(exp)     => fail(s"Source generation failed with exception: ${exp.getMessage}")
      }
      // implementation indirectly calls synthesis
      val implResults = Synthesis.runVivadoImplementation(dir)
      println(s"Implementation results: ${implResults}")
      implResults.report shouldBe defined
      implResults.lut    shouldBe defined
      implResults.lut    should equal(Some(8))
      implResults.ff     shouldBe defined
      implResults.ff     should equal(Some(0))
      implResults.dsp    shouldBe defined
      implResults.dsp    should equal(Some(0))
    }
  } else {
    println("Vivado environment not detected; skipping synthesis tests")
  }
}
