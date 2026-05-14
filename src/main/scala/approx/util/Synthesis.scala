package approx.util

import chisel3.RawModule

import circt.stage.ChiselStage

import java.nio.file.{Files, Paths}

import scala.util.{Try, Success, Failure}

object Synthesis {

  case class VivadoSynthesisResults(buildDir: String, success: Boolean, synReport: Option[String], lut: Option[Int], ff: Option[Int], dsp: Option[Int])

  case class VivadoImplementationResults(buildDir: String, success: Boolean, implReport: Option[String], lut: Option[Int], ff: Option[Int], dsp: Option[Int])

  /** Generates Vivado synthesis and implementation TCL scripts along with the
   * corresponding SystemVerilog source for a given Chisel module
   * 
   * @param gen a function that generates the Chisel module to be synthesized
   * @param part the target FPGA part number (defaults to "xc7a35t")
   * @return a Try containing a tuple of (build directory, SystemVerilog
   *         filename, synthesis TCL filename, implementation TCL filename)
   */
  def generateVivadoSources(gen: () => RawModule, part: String = "xc7a35t"): Try[(String, String, String, String)] = {
    Try {
      // Generate SystemVerilog source first to get the module name;
      // regex extraction assumes the top module is the last one defined
      val moduleNameRegex = "module\\s+([A-Za-z_]\\w*)\\s*\\(".r
      val sv = ChiselStage.emitSystemVerilog(gen(), firtoolOpts = Array("--disable-layers", "Verification"))
      val topName = moduleNameRegex.findAllMatchIn(sv)
        .map(_.group(1)).toList
        .lastOption
        .getOrElse(throw new RuntimeException("Failed to extract top module name from generated SystemVerilog"))

      // Ensure unique build directory exists
      val buildDir = Paths.get(s"build/Vivado/${topName}_${part}")
      Files.createDirectories(buildDir)

      // Generate SystemVerilog source file
      val svFile = s"${topName}.sv"
      Files.write(buildDir.resolve(svFile), sv.getBytes)

      // Generate TCL synthesis script
      val synTclFile = s"${topName}_syn.tcl"
      val synRptFile = s"${topName}_syn.rpt"
      val synDcpFile = s"${topName}_syn.dcp"
      val synTcl = s"""
        |read_verilog ${svFile}
        |synth_design -top ${topName} -part ${part}
        |report_utilization -file ${synRptFile}
        |write_checkpoint -force ${synDcpFile}
      """.stripMargin
      Files.write(buildDir.resolve(synTclFile), synTcl.getBytes)

      // Generate TCL implementation script
      val implTclFile = s"${topName}_impl.tcl"
      val implRptFile = s"${topName}_impl.rpt"
      val implDcpFile = s"${topName}_impl.dcp"
      val implTcl = s"""
        |read_checkpoint ${synDcpFile}
        |link_design
        |opt_design
        |place_design
        |route_design
        |report_utilization -file ${implRptFile}
        |write_checkpoint -force ${implDcpFile}
      """.stripMargin
      Files.write(buildDir.resolve(implTclFile), implTcl.getBytes)

      // Generate helper Makefile
      val make = s"""
        |SV_FILE            =${svFile}
        |
        |VIVADO_SYN_TCL     =${synTclFile}
        |VIVADO_SYN_REPORT  =${synRptFile}
        |
        |VIVADO_IMPL_TCL    =${implTclFile}
        |VIVADO_IMPL_REPORT =${implRptFile}
        |
        |.PHONY: clean
        |# Remove generated reports
        |clean:
        |\trm -f $${VIVADO_SYN_REPORT} $${VIVADO_IMPL_REPORT}
        |
        |.PHONY: syn
        |# Run synthesis to generate synthesis report $${VIVADO_SYN_REPORT}
        |syn: $${VIVADO_SYN_REPORT}
        |
        |$${VIVADO_SYN_REPORT}: $${SV_FILE} $${VIVADO_SYN_TCL}
        |\tvivado -mode batch -source ${synTclFile}
        |
        |.PHONY: impl
        |# Run implementation to generate implementation report $${VIVADO_IMPL_REPORT}
        |impl: $${VIVADO_IMPL_REPORT}
        |
        |$${VIVADO_IMPL_REPORT}: $${VIVADO_SYN_REPORT} $${VIVADO_IMPL_TCL}
        |\tvivado -mode batch -source ${implTclFile}
      """.stripMargin
      Files.write(buildDir.resolve("Makefile"), make.getBytes)

      (buildDir.toString, svFile, synTclFile, implTclFile)
    }
  }

  def runVivadoSynthesis(dir: String): VivadoSynthesisResults = {
    ???
  }

  def runVivadoImplementation(dir: String): VivadoImplementationResults = {
    ???
  }
}
