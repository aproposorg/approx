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
      results.synReport shouldBe defined
      results.lut       shouldBe defined
      results.ff        shouldBe defined
      results.dsp       shouldBe defined
      results.lut       should equal(Some(8))
    }
  } else {
    println("Vivado environment not detected; skipping synthesis tests")
  }
}
