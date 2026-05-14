package approx.multiplication

import chisel3._
import chisel3.simulator.scalatest.ChiselSim

import org.scalatest.flatspec.AnyFlatSpec

class AmpereishSpec extends AnyFlatSpec with ChiselSim {
  behavior of "Ampereish"

  val CommonWidths = List(4, 8, 16, 32)
  val OddWidths    = List(5, 13, 29)

  val rng = new scala.util.Random(0)

  for (width <- CommonWidths ++ OddWidths) {
    it should s"generate with width $width" in {
      // generate a random configuration for a number of cells equal to or
      // slightly lower than the actual number of cells
      val extW   = if (width <= 4) 4 else ((width + 7) / 8) * 8
      val nCells = (extW / 2) * (extW / 2)
      val nExact = rng.nextInt(nCells + 1)
      val config = Seq.fill(nCells - nExact)(rng.nextInt(5)) // unspecified = exact
      getVerilogString(new Ampereish(width, config))
    }
  }
}
