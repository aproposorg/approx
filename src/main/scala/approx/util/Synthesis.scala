package approx.util

import chisel3.RawModule

import circt.stage.ChiselStage

import java.nio.file.{Files, Paths}

import scala.util.{Try, Success, Failure}

object Synthesis {

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
      val sv = ChiselStage.emitSystemVerilog(gen())
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
      val synTcl = s"""
        |read_verilog ${svFile}
        |synth_design -top ${topName} -part ${part}
        |opt_design
        |report_utilization -file ${topName}_syn.rpt
        |write_checkpoint -force ${topName}_syn.dcp
      """.stripMargin
      Files.write(buildDir.resolve(synTclFile), synTcl.getBytes)

      // Generate TCL implementation script
      val implTclFile = s"${topName}_impl.tcl"
      val implTcl = s"""
        |read_checkpoint -force ${topName}_syn.dcp
        |opt_design
        |place_design
        |route_design
        |report_utilization -file ${topName}_impl.rpt
        |write_checkpoint -force ${topName}_impl.dcp
      """.stripMargin
      Files.write(buildDir.resolve(implTclFile), implTcl.getBytes)

      (buildDir.toString, svFile, synTclFile, implTclFile)
    }
  }
}
