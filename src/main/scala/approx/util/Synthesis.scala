package approx.util

import chisel3.RawModule

import circt.stage.ChiselStage

import java.nio.file.{Files, Paths}

import scala.util.{Try, Success, Failure}

object Synthesis {

  final val VivadoBuildDir = "build/Vivado"

  case class VivadoSynthesisResults(buildDir: String, success: Boolean, synReport: Option[String], lut: Option[Int], ff: Option[Int], dsp: Option[Int])

  case class VivadoImplementationResults(buildDir: String, success: Boolean, implReport: Option[String], lut: Option[Int], ff: Option[Int], dsp: Option[Int])

  /** Generate a helper Makefile for synthesis and implementation
   *
   * @param dir the directory where the Makefile should be created
   */
  private[Synthesis] def generateHelperMakefile(dir: String) = {
    Files.createDirectories(Paths.get(dir))
    Files.write(Paths.get(dir, "Makefile"), "include *.mk".getBytes)
  }

  /** Generates Vivado synthesis and implementation TCL scripts along with the
   * corresponding SystemVerilog source for a given Chisel module
   * 
   * @param gen a function that generates the Chisel module to be synthesized
   * @param part the target FPGA part number (defaults to "xc7a35t")
   * @return a Try containing a tuple of (build directory, SV filename)
   */
  def generateVivadoSources(gen: () => RawModule, part: String = "xc7a35t"): Try[(String, String)] = {
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
      val buildDir = Paths.get(s"${VivadoBuildDir}/${topName}_${part}")
      Files.createDirectories(buildDir)

      // Generate SystemVerilog source file
      val svFile = s"${topName}.sv"
      Files.write(buildDir.resolve(svFile), sv.getBytes)

      // Generate helper Makefile
      val make = s"""
        |SV_FILE            :=${svFile}
        |
        |VIVADO_PART        :=${part}
        |
        |VIVADO_PROJ_DIR    :=${topName}
        |VIVADO_PROJ_TCL    :=${topName}_proj.tcl
        |VIVADO_PROJ_XPR    :=$$(VIVADO_PROJ_DIR)/${topName}.xpr
        |
        |VIVADO_SYN_TCL     :=${topName}_syn.tcl
        |VIVADO_SYN_DCP     :=${topName}_syn.dcp
        |VIVADO_SYN_REPORT  :=${topName}_syn.rpt
        |
        |VIVADO_IMPL_TCL    :=${topName}_impl.tcl
        |VIVADO_IMPL_DCP    :=${topName}_impl.dcp
        |VIVADO_IMPL_REPORT :=${topName}_impl.rpt
        |
        |# Macros to generate TCL scripts for synthesis and implementation
        |define VIVADO_SYN_TCL_CONTENT
        |read_verilog $$(SV_FILE)
        |synth_design -top ${topName} -part $$(VIVADO_PART)
        |report_utilization -file $$(VIVADO_SYN_REPORT)
        |write_checkpoint -force $$(VIVADO_SYN_DCP)
        |
        |endef
        |
        |$$(VIVADO_SYN_TCL):
        |\t$$(file >$$@,$$(VIVADO_SYN_TCL_CONTENT))
        |
        |define VIVADO_IMPL_TCL_CONTENT
        |read_checkpoint $$(VIVADO_SYN_DCP)
        |link_design
        |opt_design
        |place_design
        |route_design
        |report_utilization -file $$(VIVADO_IMPL_REPORT)
        |write_checkpoint -force $$(VIVADO_IMPL_DCP)
        |
        |endef
        |
        |$$(VIVADO_IMPL_TCL):
        |\t$$(file >$$@,$$(VIVADO_IMPL_TCL_CONTENT))
        |
        |define VIVADO_PROJ_TCL_CONTENT
        |create_project -force $$(VIVADO_PROJ_XPR) -part $$(VIVADO_PART)
        |add_files $$(SV_FILE)
        |set_property top ${topName} [current_fileset]
        |update_compile_order -fileset sources_1
        |
        |endef
        |
        |$$(VIVADO_PROJ_TCL):
        |\t$$(file >$$@,$$(VIVADO_PROJ_TCL_CONTENT))
        |
        |# Helpers to generate and open a Vivado project for interactive exploration
        |$$(VIVADO_PROJ_XPR): $$(VIVADO_PROJ_TCL)
        |\tvivado -nolog -nojournal -mode batch -source $$<
        |
        |.PHONY: generate-vivado-project
        |# Generate Vivado project file
        |generate-vivado-project: $$(VIVADO_PROJ_XPR)
        |
        |.PHONY: open-vivado-gui
        |# Open Vivado GUI with the generated project
        |open-vivado-gui: $$(VIVADO_PROJ_XPR)
        |\tvivado -nolog -nojournal $$<
        |
        |.PHONY: open-vivado-tcl
        |# Open Vivado TCL with the generated project
        |open-vivado-tcl: $$(VIVADO_PROJ_XPR)
        |\tvivado -nolog -nojournal -mode tcl $$<
        |
        |# Helper clean targets for generated files
        |.PHONY: clean-vivado
        |# Remove all generated Vivado files
        |clean-vivado: clean-vivado-project clean-vivado-tcl clean-vivado-rpt clean-vivado-dcp
        |
        |.PHONY: clean-vivado-project
        |# Remove generated Vivado project files
        |clean-vivado-project:
        |\trm -rf $$(VIVADO_PROJ_DIR)
        |
        |.PHONY: clean-vivado-tcl
        |# Remove generated TCL scripts
        |clean-vivado-tcl:
        |\trm -f $$(VIVADO_PROJ_TCL) $$(VIVADO_SYN_TCL) $$(VIVADO_IMPL_TCL)
        |
        |.PHONY: clean-vivado-rpt
        |# Remove generated reports
        |clean-vivado-rpt:
        |\trm -f $$(VIVADO_SYN_REPORT) $$(VIVADO_IMPL_REPORT)
        |
        |.PHONY: clean-vivado-dcp
        |# Remove generated checkpoints
        |clean-vivado-dcp:
        |\trm -f $$(VIVADO_SYN_DCP) $$(VIVADO_IMPL_DCP)
        |
        |$$(VIVADO_SYN_REPORT): $$(SV_FILE) $$(VIVADO_SYN_TCL)
        |\tvivado -nolog -nojournal -mode batch -source $$(VIVADO_SYN_TCL)
        |
        |.PHONY: vivado-syn
        |# Run synthesis to generate synthesis report $$(VIVADO_SYN_REPORT)
        |vivado-syn: $$(VIVADO_SYN_REPORT)
        |
        |$$(VIVADO_IMPL_REPORT): $$(VIVADO_SYN_REPORT) $$(VIVADO_IMPL_TCL)
        |\tvivado -nolog -nojournal -mode batch -source $$(VIVADO_IMPL_TCL)
        |
        |.PHONY: vivado-impl
        |# Run implementation to generate implementation report $$(VIVADO_IMPL_REPORT)
        |vivado-impl: $$(VIVADO_IMPL_REPORT)
      """.stripMargin
      Files.write(buildDir.resolve("vivado.mk"), make.getBytes)

      // Generate local and common helper Makefiles
      generateHelperMakefile(buildDir.toString)

      (buildDir.toString, svFile)
    }
  }

  def runVivadoSynthesis(dir: String): VivadoSynthesisResults = {
    ???
  }

  def runVivadoImplementation(dir: String): VivadoImplementationResults = {
    ???
  }
}
