package approx.util

import java.nio.file.{Files, Paths}

import scala.util.{Try, Success, Failure}

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class SynthesisSpec extends AnyFlatSpec with Matchers {
  behavior of "Synthesis"

  it should "generate Vivado synthesis and implementation sources for an RCA adder" in {
    val (dir, sv) = Synthesis.generateVivadoSources(() => new approx.addition.RCA(8)) match {
      case Success(result)  => result
      case Failure(exp)     => fail(s"Source generation failed with exception: ${exp.getMessage}")
    }
    val svPath   = Paths.get(dir, sv)
    Files.exists(svPath)   shouldBe true
  }
}
