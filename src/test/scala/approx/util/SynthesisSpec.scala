package approx.util

import java.nio.file.{Files, Paths}

import scala.util.{Try, Success, Failure}

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class SynthesisSpec extends AnyFlatSpec with Matchers {
  behavior of "Synthesis"

  it should "generate Vivado synthesis and implementation sources for an RCA adder" in {
    val (dir, sv, syn, impl) = Synthesis.generateVivadoSources(() => new approx.addition.RCA(8)) match {
      case Success(result)  => result
      case Failure(exp)     => fail(s"Source generation failed with exception: ${exp.getMessage}")
    }
    val svPath   = Paths.get(dir, sv)
    val synPath  = Paths.get(dir, syn)
    val implPath = Paths.get(dir, impl)
    Files.exists(svPath)   shouldBe true
    Files.exists(synPath)  shouldBe true
    Files.exists(implPath) shouldBe true
  }
}
