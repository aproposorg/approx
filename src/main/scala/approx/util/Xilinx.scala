package approx.util

import chisel3._
import chisel3.experimental.{IntParam, RawParam, StringParam}

/** Collection of useful primitives for doing arithmetic with Xilinx devices
 * 
 * @note Currently includes the following device types
 *       - 7 Series (denoted by `SevenSeries`)
 *       - UltraScale (denoted by `UltraScale`)
 *       - Versal   (denoted by `Versal`)
 * 
 * @note Commonly supported primitives are included in the `Common` object.
 */
object Xilinx {
  /** Useful common primitives for doing arithmetic with Xilinx devices
   * 
   * @note Currently includes the following primitives:
   *       - LUT1
   *       - LUT2
   *       - LUT3
   *       - LUT4
   *       - LUT5
   *       - LUT6
   *       - LUT6_2
   */
  object Common {
    /** Generate an initialization string for a LUT1
     * @param fO the function to the generate the output
     * @return an initialization string for a LUT1
     */
    def genLUT1InitString(fO: Seq[Boolean] => Boolean) = genLUTInitString(fO, 1)

    class LUT1(init: String = "2'b00") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O = Output(Bool())
        val I = Input(Bool())
      })
    }

    /** Generate an initialization string for a LUT2
     * @param fO the function to the generate the output
     * @return an initialization string for a LUT2
     */
    def genLUT2InitString(fO: Seq[Boolean] => Boolean) = genLUTInitString(fO, 2)

    class LUT2(init: String = "4'h0") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O  = Output(Bool())
        val I0 = Input(Bool())
        val I1 = Input(Bool())
      })
    }

    /** Generate an initialization string for a LUT3
     * @param fO the function to the generate the output
     * @return an initialization string for a LUT3
     */
    def genLUT3InitString(fO: Seq[Boolean] => Boolean) = genLUTInitString(fO, 3)

    class LUT3(init: String = "8'h00") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O  = Output(Bool())
        val I0 = Input(Bool())
        val I1 = Input(Bool())
        val I2 = Input(Bool())
      })
    }

    /** Generate an initialization string for a LUT4
     * @param fO the function to the generate the output
     * @return an initialization string for a LUT4
     */
    def genLUT4InitString(fO: Seq[Boolean] => Boolean) = genLUTInitString(fO, 4)

    class LUT4(init: String = "16'h0000") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O  = Output(Bool())
        val I0 = Input(Bool())
        val I1 = Input(Bool())
        val I2 = Input(Bool())
        val I3 = Input(Bool())
      })
    }

    /** Generate an initialization string for a LUT5
     * @param fO the function to the generate the output
     * @return an initialization string for a LUT5
     */
    def genLUT5InitString(fO: Seq[Boolean] => Boolean) = genLUTInitString(fO, 5)

    class LUT5(init: String = "32'h00000000") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O  = Output(Bool())
        val I0 = Input(Bool())
        val I1 = Input(Bool())
        val I2 = Input(Bool())
        val I3 = Input(Bool())
        val I4 = Input(Bool())
      })
    }

    /** Generate an initialization string for a LUT6
     * @param fO the function to the generate the output
     * @return an initialization string for a LUT6
     */
    def genLUT6InitString(fO: Seq[Boolean] => Boolean) = genLUTInitString(fO, 6)

    class LUT6(init: String = "64'h0000000000000000") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O  = Output(Bool())
        val I0 = Input(Bool())
        val I1 = Input(Bool())
        val I2 = Input(Bool())
        val I3 = Input(Bool())
        val I4 = Input(Bool())
        val I5 = Input(Bool())
      })
    }

    /** Generate an initialization string for a LUTx with `x` in the range 1 to 6
     * @param fO the function to the generate the output
     * @return an initialization string for a LUTx
     */
    private def genLUTInitString(fO: Seq[Boolean] => Boolean, x: Int): String = {
      require(1 <= x && x <= 6, "can only generate initialization strings for LUTs with 1 to 6 inputs")
      val width = 1 << x
      val tTableO = (0 until width).foldLeft(Array.empty[Boolean]) { case (acc, i) =>
        val inputs = (0 until 6).map(shft => ((i >> shft) & 0x1) == 1)
        acc :+ fO(inputs)
      }
      val init = tTableO.zipWithIndex.filter(_._1).foldLeft(BigInt(0)) { case (acc, (_, pos)) =>
        acc.setBit(pos)
      }
      s"${width}'h${init.toString(16)}"
    }

    /** Generate an initialization string for a LUT6_2
     * @param fO5 the function to generate the O5 output
     * @param fO6 the function to generate the O6 output
     * @return an initialization string for a LUT6_2
     */
    def genLUT6_2InitString(fO5: Seq[Boolean] => Boolean, fO6: Seq[Boolean] => Boolean): String = {
      val (tTableO5, tTableO6) = (32 until 64).foldLeft((Array.empty[Boolean], Array.empty[Boolean])) {
        case ((accO5, accO6), i) =>
        val inputs = (0 until 6).map(shft => ((i >> shft) & 0x1) == 1)
        (accO5 :+ fO5(inputs), accO6 :+ fO6(inputs))
      }
      val init = (tTableO5 ++ tTableO6).zipWithIndex.filter(_._1).foldLeft(BigInt(0)) {
        case (acc, (_, pos)) =>
        acc.setBit(pos)
      }
      s"64'h${init.toString(16)}"
    }

    class LUT6_2(init: String = "64'h0000000000000000") extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val O6 = Output(Bool())
        val O5 = Output(Bool())
        val I0 = Input(Bool())
        val I1 = Input(Bool())
        val I2 = Input(Bool())
        val I3 = Input(Bool())
        val I4 = Input(Bool())
        val I5 = Input(Bool())
      })
    }
  }

  /** Useful primitives for doing arithmetic with 7 Series devices
   * 
   * @note Currently includes the following primitives:
   *       - CARRY4
   *       - DSP48E1
   * 
   * @note Documentation for these is available in 
   *       https://docs.xilinx.com/v/u/2012.2-English/ug953-vivado-7series-libraries
   */
  object SevenSeries {
    class CARRY4 extends BlackBox {
      val io = IO(new Bundle {
        val CO     = Output(UInt(4.W))
        val O      = Output(UInt(4.W))
        val CI     = Input(Bool())
        val CYINIT = Input(Bool())
        val DI     = Input(UInt(4.W))
        val S      = Input(UInt(4.W))
      })
    }

    class DSP48E1(aCascReg: Int = 1, aDReg: Int = 1, aInput: String = "DIRECT",
                  aluModeReg: Int = 1, aReg: Int = 1, autoResetPatDet: String = "NO_RESET",
                  bCascReg: Int = 1, bInput: String = "DIRECT", bReg: Int = 1,
                  carryInReg: Int = 1, carryInSelReg: Int = 1, cReg: Int = 1, dReg: Int = 1,
                  inModeReg: Int = 1, mask: String = "48'h3FFFFFFFFFFF", mReg: Int = 1,
                  opModeReg: Int = 1, pattern: String = "48'h000000000000", pReg: Int = 1,
                  selMask: String = "MASK", selPattern: String = "PATTERN",
                  useDPort: String = "FALSE", useMult: String = "MULTIPLY",
                  usePatternDetect: String = "NO_PATDET", useSimd: String = "ONE48")
      extends BlackBox(Map(
        "ACASCREG" -> IntParam(aCascReg),
        "ADREG" -> IntParam(aDReg),
        "A_INPUT" -> StringParam(aInput),
        "ALUMODEREG" -> IntParam(aluModeReg),
        "AREG" -> IntParam(aReg),
        "AUTORESET_PATDET" -> StringParam(autoResetPatDet),
        "BCASCREG" -> IntParam(bCascReg),
        "B_INPUT" -> StringParam(bInput),
        "BREG" -> IntParam(bReg),
        "CARRYINREG" -> IntParam(carryInReg),
        "CARRYINSELREG" -> IntParam(carryInSelReg),
        "CREG" -> IntParam(cReg),
        "DREG" -> IntParam(dReg),
        "INMODEREG" -> IntParam(inModeReg),
        "MASK" -> RawParam(mask),
        "MREG" -> IntParam(mReg),
        "OPMODEREG" -> IntParam(opModeReg),
        "PATTERN" -> RawParam(pattern),
        "PREG" -> IntParam(pReg),
        "SEL_MASK" -> StringParam(selMask),
        "SEL_PATTERN" -> StringParam(selPattern),
        "USE_DPORT" -> StringParam(useDPort),
        "USE_MULT" -> StringParam(useMult),
        "USE_PATTERN_DETECT" -> StringParam(usePatternDetect),
        "USE_SIMD" -> StringParam(useSimd)
      )) {
      val io = IO(new Bundle {
        val A = Input(UInt(30.W))
        val ACIN = Input(UInt(30.W))
        val ACOUT = Output(UInt(30.W))
        val ALUMODE = Input(UInt(4.W))
        val B = Input(UInt(18.W))
        val BCIN = Input(UInt(18.W))
        val BCOUT = Output(UInt(18.W))
        val C = Output(UInt(48.W))
        val CARRYCASCIN = Input(Bool())
        val CARRYCASCOUT = Output(Bool())
        val CARRYIN = Input(Bool())
        val CARRYINSEL = Input(UInt(3.W))
        val CARRYOUT = Output(UInt(4.W))
        val CEAD = Input(Bool())
        val CEALUMODE = Input(Bool())
        val CEA1 = Input(Bool())
        val CEA2 = Input(Bool())
        val CEB1 = Input(Bool())
        val CEB2 = Input(Bool())
        val CEC = Input(Bool())
        val CECARRYIN = Input(Bool())
        val CECTRL = Input(Bool())
        val CED = Input(Bool())
        val CEINMODE = Input(Bool())
        val CEM = Input(Bool())
        val CEP = Input(Bool())
        val CLK = Input(Clock())
        val D = Input(UInt(25.W))
        val INMODE = Input(UInt(5.W))
        val MULTSIGNIN = Input(Bool())
        val MULTSIGNOUT = Output(Bool())
        val OPMODE = Input(UInt(7.W))
        val OVERFLOW = Output(Bool())
        val P = Output(UInt(48.W))
        val PATTERNBDETECT = Output(Bool())
        val PATTERNDETECT = Output(Bool())
        val PCIN = Input(UInt(48.W))
        val PCOUT = Output(UInt(48.W))
        val RSTA = Input(Bool())
        val RSTALLCARRYIN = Input(Bool())
        val RSTALUMODE = Input(Bool())
        val RSTB = Input(Bool())
        val RSTC = Input(Bool())
        val RSTCTRL = Input(Bool())
        val RSTD = Input(Bool())
        val RSTINMODE = Input(Bool())
        val RSTM = Input(Bool())
        val RSTP = Input(Bool())
        val UNDERFLOW = Output(Bool())
      })
    }
  }

  /** Useful primitives for doing arithmetic with UltraScale devices
   * 
   * @note Currently includes the following primitives:
   *       - CARRY8
   *       - DSP48E2
   * 
   * @note Documentation for these is available in 
   *       https://docs.xilinx.com/v/u/2018.1-English/ug974-vivado-ultrascale-libraries
   */
  object UltraScale {
    class CARRY8(carryType: String = "SINGLE_CY8") extends BlackBox(Map("CARRY_TYPE" -> StringParam(carryType))) {
      val io = IO(new Bundle {
        val CI     = Input(Bool())
        val CI_TOP = Input(Bool())
        val CO     = Output(UInt(8.W))
        val DI     = Input(UInt(8.W))
        val O      = Output(UInt(8.W))
        val S      = Input(UInt(8.W))
      })
    }

    class DSP48E2(aInput: String = "DIRECT", aMultSel: String = "A", bInput: String = "DIRECT", 
                  bMultSel: String = "B", preAddInSel: String = "A", rnd: String = "48'h000000000000",
                  useMult: String = "MULTIPLY", useSimd: String = "ONE48", useWideXor: String = "FALSE",
                  xorSimd: String = "XOR_24_48_96", autoResetPatDet: String = "NO_RESET",
                  autoResetPriority: String = "RESET", mask: String = "48'h3FFFFFFFFFFF",
                  pattern: String = "48'h000000000000", selMask: String = "MASK",
                  selPattern: String = "PATTERN", usePatternDetect: String = "NO_PATDET",
                  isAluModeInverted: String = "4'b0000", isCarryInInverted: String = "1'b0",
                  isClkInverted: String = "1'b0", isInModeInverted: String = "5'b00000",
                  isOpModeInverted: String = "9'b000000000", isRstAInverted: String = "1'b0",
                  isRstAllCarryInInverted: String = "1'b0", isRstAluModeInverted: String = "1'b0",
                  isRstBInverted: String = "1'b0", isRstCInverted: String = "1'b0",
                  isRstCtrlInverted: String = "1'b0", isRstDInverted: String = "1'b0",
                  isRstInModeInverted: String = "1'b0", isRstMInverted: String = "1'b0",
                  isRstPInverted: String = "1'b0", aCascReg: Int = 1, aDReg: Int = 1,
                  aluModeReg: Int = 1, aReg: Int = 1, bCascReg: Int = 1, bReg: Int = 1,
                  carryInReg: Int = 1, carryInSelReg: Int = 1, cReg: Int = 1, dReg: Int = 1,
                  inModeReg: Int = 1,  mReg: Int = 1, opModeReg: Int = 1, pReg: Int = 1)
      extends BlackBox(Map(
        "A_INPUT" -> StringParam(aInput),
        "AMULTSEL" -> StringParam(aMultSel),
        "B_INPUT" -> StringParam(bInput),
        "BMULTSEL" -> StringParam(bMultSel),
        "PREADDINSEL" -> StringParam(preAddInSel),
        "RND" -> RawParam(rnd),
        "USE_MULT" -> StringParam(useMult),
        "USE_SIMD" -> StringParam(useSimd),
        "USE_WIDEXOR" -> StringParam(useWideXor),
        "XORSIMD" -> StringParam(xorSimd),
        "AUTORESET_PATDET" -> StringParam(autoResetPatDet),
        "AUTORESET_PRIORITY" -> StringParam(autoResetPriority),
        "MASK" -> RawParam(mask),
        "PATTERN" -> RawParam(pattern), 
        "SEL_MASK" -> StringParam(selMask),
        "SEL_PATTERN" -> StringParam(selPattern),
        "USE_PATTERN_DETECT" -> StringParam(usePatternDetect),
        "IS_ALUMODE_INVERTED" -> RawParam(isAluModeInverted),
        "IS_CARRYIN_INVERTED" -> RawParam(isCarryInInverted),
        "IS_CLK_INVERTED" -> RawParam(isClkInverted),
        "IS_INMODE_INVERTED" -> RawParam(isInModeInverted),
        "IS_OPMODE_INVERTED" -> RawParam(isOpModeInverted),
        "IS_RSTA_INVERTED" -> RawParam(isRstAInverted),
        "IS_RSTALLCARRYIN_INVERTED" -> RawParam(isRstAllCarryInInverted),
        "IS_RSTALUMODE_INVERTED" -> RawParam(isRstAluModeInverted),
        "IS_RSTB_INVERTED" -> RawParam(isRstBInverted),
        "IS_RSTC_INVERTED" -> RawParam(isRstCInverted),
        "IS_RSTCTRL_INVERTED" -> RawParam(isRstCtrlInverted),
        "IS_RSTD_INVERTED" -> RawParam(isRstDInverted),
        "IS_RSTINMODE_INVERTED" -> RawParam(isRstInModeInverted),
        "IS_RSTM_INVERTED" -> RawParam(isRstMInverted),
        "IS_RSTP_INVERTED" -> RawParam(isRstPInverted),
        "ACASCREG" -> IntParam(aCascReg),
        "ADREG" -> IntParam(aDReg),
        "ALUMODEREG" -> IntParam(aluModeReg),
        "AREG" -> IntParam(aReg),
        "BCASCREG" -> IntParam(bCascReg),
        "BREG" -> IntParam(bReg),
        "CARRYINREG" -> IntParam(carryInReg),
        "CARRYINSELREG" -> IntParam(carryInSelReg),
        "CREG" -> IntParam(cReg),
        "DREG" -> IntParam(dReg),
        "INMODEREG" -> IntParam(inModeReg),
        "MREG" -> IntParam(mReg),
        "OPMODEREG" -> IntParam(opModeReg),
        "PREG" -> IntParam(pReg)
      )) {
      val io = IO(new Bundle {
        val XOROUT = Output(UInt(8.W))
        val ACIN = Input(UInt(30.W))
        val ACOUT = Output(UInt(30.W))
        val BCIN = Input(UInt(18.W))
        val BCOUT = Output(UInt(18.W))
        val CARRYCASCIN = Input(Bool())
        val CARRYCASCOUT = Output(Bool())
        val MULTSIGNIN = Input(Bool())
        val MULTSIGNOUT = Output(Bool())
        val PCIN = Input(UInt(48.W))
        val PCOUT = Output(UInt(48.W))
        val ALUMODE = Input(UInt(4.W))
        val CARRYINSEL = Input(UInt(3.W))
        val CLK = Input(Clock())
        val INMODE = Input(UInt(5.W))
        val OPMODE = Input(UInt(9.W))
        val OVERFLOW = Output(Bool())
        val PATTERNBDETECT = Output(Bool())
        val PATTERNDETECT = Output(Bool())
        val UNDERFLOW = Output(Bool())
        val A = Input(UInt(30.W))
        val B = Input(UInt(18.W))
        val C = Output(UInt(48.W))
        val CARRYIN = Input(Bool())
        val CARRYOUT = Output(UInt(4.W))
        val D = Input(UInt(27.W))
        val P = Output(UInt(48.W))
        val CEAD = Input(Bool())
        val CEALUMODE = Input(Bool())
        val CEA1 = Input(Bool())
        val CEA2 = Input(Bool())
        val CEB1 = Input(Bool())
        val CEB2 = Input(Bool())
        val CEC = Input(Bool())
        val CECARRYIN = Input(Bool())
        val CECTRL = Input(Bool())
        val CED = Input(Bool())
        val CEINMODE = Input(Bool())
        val CEM = Input(Bool())
        val CEP = Input(Bool())
        val RSTA = Input(Bool())
        val RSTALLCARRYIN = Input(Bool())
        val RSTALUMODE = Input(Bool())
        val RSTB = Input(Bool())
        val RSTC = Input(Bool())
        val RSTCTRL = Input(Bool())
        val RSTD = Input(Bool())
        val RSTINMODE = Input(Bool())
        val RSTM = Input(Bool())
        val RSTP = Input(Bool())
      })
    }
  }

  /** Useful primitives for doing arithmetic with Versal devices
   * 
   * @note Currently includes the following primitives:
   *       - LUT6CY
   *       - LOOKAHEAD8
   *       - DSP58
   * 
   * @note Documentation for these is available in 
   *       https://docs.xilinx.com/viewer/book-attachment/xpIylNRZbUxJCzqBIAu5oQ/J9YbjhhnJdPNUarvS5Zcww
   */
  object Versal {
    /** Generate an initialization string for a LUT6CY
     * @param fO51 the function to generate the O51 output
     * @param fO52 the function to generate the O52 output
     * @return an initialization string for a LUT6CY
     * 
     * @note The least significant half of the truth table of `fO51` constitutes the 
     *       function of the `PROP` output.
     */
    def genLUT6CYInitString(fO51: Seq[Boolean] => Boolean, fO52: Seq[Boolean] => Boolean): String = {
      val (tTableO51, tTableO52) = (32 until 64).foldLeft((Array.empty[Boolean], Array.empty[Boolean])) {
        case ((accO51, accO52), i) =>
        val inputs = (0 until 6).map(shft => ((i >> shft) & 0x1) == 1)
        (accO51 :+ fO51(inputs), accO52 :+ fO52(inputs))
      }
      val init = (tTableO51 ++ tTableO52).zipWithIndex.filter(_._1).foldLeft(BigInt(0)) {
        case (acc, (_, pos)) =>
        acc.setBit(pos)
      }
      s"64'h${init.toString(16)}"
    }

    class LUT6CY(init: String) extends BlackBox(Map("INIT" -> RawParam(init))) {
      val io = IO(new Bundle {
        val I0   = Input(Bool())
        val I1   = Input(Bool())
        val I2   = Input(Bool())
        val I3   = Input(Bool())
        val I4   = Input(Bool())
        val O51  = Output(Bool())
        val O52  = Output(Bool())
        val PROP = Output(Bool())
      })
    }

    class LOOKAHEAD8(lookB: String, lookD: String, lookF: String, lookH: String)
      extends BlackBox(Map("LOOKB" -> StringParam(lookB), "LOOKD" -> StringParam(lookD),
                           "LOOKF" -> StringParam(lookF), "LOOKH" -> StringParam(lookH))) {
      val io = IO(new Bundle {
        val CIN = Input(Bool())
        val COUTB = Output(Bool())
        val COUTD = Output(Bool())
        val COUTF = Output(Bool())
        val COUTH = Output(Bool())
        val CYA   = Input(Bool())
        val CYB   = Input(Bool())
        val CYC   = Input(Bool())
        val CYD   = Input(Bool())
        val CYE   = Input(Bool())
        val CYF   = Input(Bool())
        val CYG   = Input(Bool())
        val CYH   = Input(Bool())
        val PROPA = Input(Bool())
        val PROPB = Input(Bool())
        val PROPC = Input(Bool())
        val PROPD = Input(Bool())
        val PROPE = Input(Bool())
        val PROPF = Input(Bool())
        val PROPG = Input(Bool())
        val PROPH = Input(Bool())
      })
    }

    class DSP58(aInput: String = "DIRECT", aMultSel: String = "A", bInput: String = "DIRECT", 
                bMultSel: String = "B", dspMode: String = "INT24", preAddInSel: String = "A",
                rnd: String = "58'h000000000000000", useMult: String = "MULTIPLY",
                useSimd: String = "ONE58", useWideXor: String = "FALSE",
                xorSimd: String = "XOR24_34_58_116", autoResetPatDet: String = "NO_RESET",
                autoResetPriority: String = "RESET", mask: String = "58'h0FFFFFFFFFFFFF",
                pattern: String = "58'h000000000000000", selMask: String = "MASK",
                selPattern: String = "PATTERN", usePatternDetect: String = "NO_PATDET",
                isAluModeInverted: String = "4'b0000", isAsyncRstInverted: String = "1'b0",
                isCarryInInverted: String = "1'b0", isClkInverted: String = "1'b0",
                isInModeInverted: String = "5'b00000", isNegateInverted: String = "3'b000",
                isOpModeInverted: String = "9'b000000000", isRstAInverted: String = "1'b0",
                isRstAllCarryInInverted: String = "1'b0", isRstAluModeInverted: String = "1'b0",
                isRstBInverted: String = "1'b0", isRstCInverted: String = "1'b0",
                isRstCtrlInverted: String = "1'b0", isRstDInverted: String = "1'b0",
                isRstInModeInverted: String = "1'b0", isRstMInverted: String = "1'b0",
                isRstPInverted: String = "1'b0", aCascReg: Int = 1, aDReg: Int = 1,
                aluModeReg: Int = 1, aReg: Int = 1, bCascReg: Int = 1, bReg: Int = 1,
                carryInReg: Int = 1, carryInSelReg: Int = 1, cReg: Int = 1, dReg: Int = 1,
                inModeReg: Int = 1,  mReg: Int = 1, opModeReg: Int = 1, pReg: Int = 1,
                resetMode: String = "SYNC")
        extends BlackBox(Map(
          "A_INPUT" -> StringParam(aInput),
          "AMULTSEL" -> StringParam(aMultSel),
          "B_INPUT" -> StringParam(bInput),
          "BMULTSEL" -> StringParam(bMultSel),
          "DSP_MODE" -> StringParam(dspMode),
          "PREADDINSEL" -> StringParam(preAddInSel),
          "RND" -> RawParam(rnd),
          "USE_MULT" -> StringParam(useMult),
          "USE_SIMD" -> StringParam(useSimd),
          "USE_WIDEXOR" -> StringParam(useWideXor),
          "XORSIMD" -> StringParam(xorSimd),
          "AUTORESET_PATDET" -> StringParam(autoResetPatDet),
          "AUTORESET_PRIORITY" -> StringParam(autoResetPriority),
          "MASK" -> RawParam(mask),
          "PATTERN" -> RawParam(pattern), 
          "SEL_MASK" -> StringParam(selMask),
          "SEL_PATTERN" -> StringParam(selPattern),
          "USE_PATTERN_DETECT" -> StringParam(usePatternDetect),
          "IS_ALUMODE_INVERTED" -> RawParam(isAluModeInverted),
          "IS_ASYNC_RST_INVERTED" -> RawParam(isAsyncRstInverted),
          "IS_CARRYIN_INVERTED" -> RawParam(isCarryInInverted),
          "IS_CLK_INVERTED" -> RawParam(isClkInverted),
          "IS_INMODE_INVERTED" -> RawParam(isInModeInverted),
          "IS_NEGATE_INVERTED" -> RawParam(isNegateInverted),
          "IS_OPMODE_INVERTED" -> RawParam(isOpModeInverted),
          "IS_RSTA_INVERTED" -> RawParam(isRstAInverted),
          "IS_RSTALLCARRYIN_INVERTED" -> RawParam(isRstAllCarryInInverted),
          "IS_RSTALUMODE_INVERTED" -> RawParam(isRstAluModeInverted),
          "IS_RSTB_INVERTED" -> RawParam(isRstBInverted),
          "IS_RSTC_INVERTED" -> RawParam(isRstCInverted),
          "IS_RSTCTRL_INVERTED" -> RawParam(isRstCtrlInverted),
          "IS_RSTD_INVERTED" -> RawParam(isRstDInverted),
          "IS_RSTINMODE_INVERTED" -> RawParam(isRstInModeInverted),
          "IS_RSTM_INVERTED" -> RawParam(isRstMInverted),
          "IS_RSTP_INVERTED" -> RawParam(isRstPInverted),
          "ACASCREG" -> IntParam(aCascReg),
          "ADREG" -> IntParam(aDReg),
          "ALUMODEREG" -> IntParam(aluModeReg),
          "AREG" -> IntParam(aReg),
          "BCASCREG" -> IntParam(bCascReg),
          "BREG" -> IntParam(bReg),
          "CARRYINREG" -> IntParam(carryInReg),
          "CARRYINSELREG" -> IntParam(carryInSelReg),
          "CREG" -> IntParam(cReg),
          "DREG" -> IntParam(dReg),
          "INMODEREG" -> IntParam(inModeReg),
          "MREG" -> IntParam(mReg),
          "OPMODEREG" -> IntParam(opModeReg),
          "PREG" -> IntParam(pReg),
          "RESET_MODE" -> StringParam(resetMode)
      )) {
      val io = IO(new Bundle {
        val XOROUT = Output(UInt(8.W))
        val ACIN = Input(UInt(34.W))
        val ACOUT = Output(UInt(34.W))
        val BCIN = Input(UInt(24.W))
        val BCOUT = Output(UInt(24.W))
        val CARRYCASCIN = Input(Bool())
        val CARRYCASCOUT = Output(Bool())
        val MULTSIGNIN = Input(Bool())
        val MULTSIGNOUT = Output(Bool())
        val PCIN = Input(UInt(58.W))
        val PCOUT = Output(UInt(58.W))
        val ALUMODE = Input(UInt(4.W))
        val CARRYINSEL = Input(UInt(3.W))
        val CLK = Input(Clock())
        val INMODE = Input(UInt(5.W))
        val NEGATE = Input(UInt(3.W))
        val OPMODE = Input(UInt(9.W))
        val OVERFLOW = Output(Bool())
        val PATTERNBDETECT = Output(Bool())
        val PATTERNDETECT = Output(Bool())
        val UNDERFLOW = Output(Bool())
        val A = Input(UInt(34.W))
        val B = Input(UInt(24.W))
        val C = Output(UInt(58.W))
        val CARRYIN = Input(Bool())
        val CARRYOUT = Output(UInt(4.W))
        val D = Input(UInt(27.W))
        val P = Output(UInt(58.W))
        val ASYNC_RST = Input(Bool())
        val CEAD = Input(Bool())
        val CEALUMODE = Input(Bool())
        val CEA1 = Input(Bool())
        val CEA2 = Input(Bool())
        val CEB1 = Input(Bool())
        val CEB2 = Input(Bool())
        val CEC = Input(Bool())
        val CECARRYIN = Input(Bool())
        val CECTRL = Input(Bool())
        val CED = Input(Bool())
        val CEINMODE = Input(Bool())
        val CEM = Input(Bool())
        val CEP = Input(Bool())
        val RSTA = Input(Bool())
        val RSTALLCARRYIN = Input(Bool())
        val RSTALUMODE = Input(Bool())
        val RSTB = Input(Bool())
        val RSTC = Input(Bool())
        val RSTCTRL = Input(Bool())
        val RSTD = Input(Bool())
        val RSTINMODE = Input(Bool())
        val RSTM = Input(Bool())
        val RSTP = Input(Bool())
      })
    }
  }
}
