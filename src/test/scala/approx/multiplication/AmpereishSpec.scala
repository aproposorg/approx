package approx.multiplication

import chisel3._
import chisel3.simulator.scalatest.ChiselSim

import org.scalatest.flatspec.AnyFlatSpec

class AmpereishSpec extends AnyFlatSpec with ChiselSim {
  behavior of "Ampereish"

  val CommonWidths = List(4, 8, 16, 32)
  val OddWidths    = List(5, 13, 29)

  for (width <- CommonWidths ++ OddWidths) {
    it should s"generate with width $width" in {
      getVerilogString(new Ampereish(width))
    }
  }
}
