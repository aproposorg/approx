package approx.util

import chisel3.RawModule

import circt.stage.ChiselStage

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters._
import scala.sys.process._
import scala.util.{Try, Success, Failure}

object Synthesis {

  final val BuildDir       = "build"
  final val VivadoBuildDir = s"${BuildDir}/Vivado"

  final val VerilogModuleNameRegex      = """module\s+([A-Za-z_]\w*)\s*\(""".r

  final val VivadoSynthesisLUTRegex      = """\|\s*Slice\s+LUTs\*\s*\|\s*(\d+)\s*\|""".r
  final val VivadoImplementationLUTRegex = """\|\s*Slice\s+LUTs\s*\|\s*(\d+)\s*\|""".r
  final val VivadoFFRegex                = """\|\s*Slice\s+Registers\s*\|\s*(\d+)\s*\|""".r
  final val VivadoDSPRegex               = """\|\s*DSPs\s*\|\s*(\d+)\s*\|""".r

  case class VivadoSynthesisResults(buildDir: String, report: Option[String], lut: Option[Int], ff: Option[Int], dsp: Option[Int])

  case class VivadoImplementationResults(buildDir: String, report: Option[String], lut: Option[Int], ff: Option[Int], dsp: Option[Int])

  /** Generate a helper Makefile for synthesis and implementation
   *
   * @param dir the directory where the Makefile should be created
   *
   * TODO extend with help and print-all-variables targets
   */
  private[Synthesis] def generateHelperMakefile(dir: String) = {
    Files.createDirectories(Paths.get(dir))
    Files.write(Paths.get(dir, "Makefile"), "include *.mk".getBytes)
  }

  /** Run a make target in a given directory
   *
   * @param dir the directory where the Makefile is located
   * @param target the make target to run
   * @return a tuple of (exit code, stdout/stderr content)
   */
  private[Synthesis] def runMakeTarget(dir: String, target: String): (Int, String) = {
    println(s"Running make target: make -C ${dir} ${target}")
    val stdout   = new StringBuilder
    val logger   = ProcessLogger(line => stdout.append(line).append("\n"))
    val exitCode = Process(Seq("make", "-C", dir, target)).!(logger)
    (exitCode, stdout.toString)
  }

  /** Get (the first) file in a given directory
   *
   * @param dir the directory to search
   * @param pattern a pattern to match files against
   * @return an Option containing the first matching file path, if any
   */
  private[Synthesis] def getFileInDir(dir: String, pattern: String): Option[Path] = {
    val stream = Files.newDirectoryStream(Paths.get(dir), pattern)
    try {
      stream.iterator().asScala.toList.headOption
    } finally {
      stream.close()
    }
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
      val sv = ChiselStage.emitSystemVerilog(gen(), firtoolOpts = Array("--disable-layers", "Verification"))
      val topName = VerilogModuleNameRegex.findAllMatchIn(sv)
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

  /** Runs Vivado synthesis using the generated Makefile and parses the
   * resulting synthesis report to extract and bundle resource utilization
   * metrics into a structured result
   *
   * @param dir the directory containing the generated Makefile and sources
   * @return a [[VivadoSynthesisResults]] instance including status and
   *         resource utilization metrics, if available
   */
  def runVivadoSynthesis(dir: String): VivadoSynthesisResults = {
    // Attempt to launch Vivado synthesis using the generated Makefile assumed
    // to exist under dir
    val (exitCode, stdout) = runMakeTarget(dir, "vivado-syn")
    if (exitCode != 0) {
      println(s"Vivado synthesis failed with exit code $exitCode")
      println(s"Vivado output:\n${stdout}")
      return VivadoSynthesisResults(dir, None, None, None, None)
    }

    // Parse synthesis report to extract resource utilization
    val synRptOpt = getFileInDir(dir, "*_syn.rpt")
    synRptOpt match {
      case None =>
        println(s"Synthesis report not found in directory: $dir")
        return VivadoSynthesisResults(dir, None, None, None, None)
      case _ =>
    }

    // Read the synthesis report and extract LUT, FF, and DSP counts using regex
    val synRptContent = new String(Files.readAllBytes(synRptOpt.get))
    val lutCount = VivadoSynthesisLUTRegex.findFirstMatchIn(synRptContent).map(_.group(1).toInt)
    val ffCount  = VivadoFFRegex          .findFirstMatchIn(synRptContent).map(_.group(1).toInt)
    val dspCount = VivadoDSPRegex         .findFirstMatchIn(synRptContent).map(_.group(1).toInt)
    VivadoSynthesisResults(dir, synRptOpt.map(_.toString), lutCount, ffCount, dspCount)
  }

  /** Runs Vivado implementation using the generated Makefile and parses the
   * resulting implementation report to extract and bundle resource utilization
   * metrics into a structured result
   *
   * @param dir the directory containing the generated Makefile and sources
   * @return a [[VivadoImplementationResults]] instance including status and
   *         resource utilization metrics, if available
   */
  def runVivadoImplementation(dir: String): VivadoImplementationResults = {
    // Attempt to launch Vivado implementation using the generated Makefile
    // assumed to exist under dir
    val (exitCode, stdout) = runMakeTarget(dir, "vivado-impl")
    if (exitCode != 0) {
      println(s"Vivado implementation failed with exit code $exitCode")
      println(s"Vivado output:\n${stdout}")
      return VivadoImplementationResults(dir, None, None, None, None)
    }

    // Parse implementation report to extract resource utilization
    val implRptOpt = getFileInDir(dir, "*_impl.rpt")
    implRptOpt match {
      case None =>
        println(s"Implementation report not found in directory: $dir")
        return VivadoImplementationResults(dir, None, None, None, None)
      case _ =>
    }

    // Read the implementation report and extract LUT, FF, and DSP counts using regex
    val implRptContent = new String(Files.readAllBytes(implRptOpt.get))
    val lutCount = VivadoImplementationLUTRegex.findFirstMatchIn(implRptContent).map(_.group(1).toInt)
    val ffCount  = VivadoFFRegex               .findFirstMatchIn(implRptContent).map(_.group(1).toInt)
    val dspCount = VivadoDSPRegex              .findFirstMatchIn(implRptContent).map(_.group(1).toInt)
    VivadoImplementationResults(dir, implRptOpt.map(_.toString), lutCount, ffCount, dspCount)
  }
}
