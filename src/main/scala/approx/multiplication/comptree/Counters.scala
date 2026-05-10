package approx.multiplication.comptree

import chisel3._
import chisel3.util.MixedVec
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}

import approx.multiplication._
import approx.multiplication.comptree.State

import approx.util.Xilinx.Common.{genLUT6InitString, genLUT6_2InitString, LUT6, LUT6_2}
import approx.util.Xilinx.SevenSeries.CARRY4
import approx.util.Xilinx.Versal.{genLUT6CYInitString, LOOKAHEAD8, LUT6CY}

/** Collection of atoms and counters useful for compressor tree
 * generation for different devices. We currently include the
 * following device types:
 * - ASIC         (denoted by `ASIC`)
 * - Xilinx FPGAs (denoted by `Xilinx.{SevenSeries, Versal}`)
 * - Intel FPGAs  (denoted by `Intel`)
 * 
 * Atoms can (and should) be composed arbitrarily to form larger counters.
 * For simplicity, we only consider compositions of two counters. All counter
 * libraries per default include half adders and full adders.
 * 
 * @todo Consider entirely reworking how composed counters are constructed.
 */
private[comptree] object Counters {
  /** Abstract atom class
   * 
   * @param luts number of LUTs consumed by this atom (if applicable)
   * 
   * Atoms are sub-elements of counters.
   */
  private[Counters] abstract class Atom(val luts: Int = -1)

  /** Represents the type of a generic counter
   * 
   * We consider two different types:
   * - Exact counters that compute proper sums and carries
   * - Approximate counters that compute inexact sums and carries
   */
  private[Counters] abstract trait CounterType

  /** Represents an exact `CounterType` */
  private[Counters] trait Exact extends CounterType

  /** Represents an approximate `CounterType` */
  private[Counters] trait Approximate extends CounterType

  /** Abstract counter class
   * 
   * @param sig the signature of the counter
   * @param cost the hardware cost of the counter (for FPGAs: no. of LUTs,
   *             for ASICs: ~number of XORs)
   * 
   * Counters can be composed from multiple atoms. All counters
   * also possess signatures that define their connectivity.
   */
  abstract class Counter(val sig: (Array[Int], Array[Int]), cost: Int = -1) {
    this: CounterType =>

    /** The strength of the counter
     * 
     * We use the definition by Preusser [2017]
     */
    val strength: Double = sig._1.sum.toDouble / sig._2.sum

    /** The efficiency of the counter
     * 
     * We use the definition by Preusser [2017] and adapt it somewhat for
     * Intel FPGAs and ASICs.
     */
    val efficiency: Double = if (cost == -1) Double.MinValue else (sig._1.sum - sig._2.sum).toDouble / cost
  }

  /** Abstract variable-length counter class
   * 
   * @param inSigFn function to compute the input signature for a given length
   * @param outSigFn function to compute the output signature for a given length
   * @param costFn function to compute the hardware cost of the counter for a
   *               given length (for FPGAs: no. of LUTs, for ASICs: ~number of XORs)
   * 
   * Variable-length counters are counters that can be cascaded to form
   * arbitrarily long chains. They are defined by a triple of functions
   * that define the input and output signatures and cost for a given length.
   */
  abstract class VarLenCounter(
    val inSigFn : Int => Array[Int],
    val outSigFn: Int => Array[Int],
    val costFn  : Int => Int = (n: Int) => -1
  ) {
    this: CounterType =>

    /** Returns the strength of the counter for length `n`
     * 
     * @param n the length of the counter
     */
    def strength(n: Int): Double = inSigFn(n).sum.toDouble / outSigFn(n).sum

    /** Returns the efficiency of the counter for length `n`
     * 
     * @param n the length of the counter
     * 
     * We use the definition by Preusser [2017] and adapt it somewhat for
     * Intel FPGAs and ASICs.
     */
    def efficiency(n: Int): Double = {
      if (costFn(n) == -1) Double.MinValue
      else (inSigFn(n).sum - outSigFn(n).sum).toDouble / costFn(n)
    }
  }

  /** Returns true iff the counter mixes in `Approximate` */
  def isApproximate(ctr: Counter): Boolean = ctr match {
    case _: Counter with Approximate => true
    case _ => false
  }

  /** Returns true iff the variable-length counter mixes in `Approximate` */
  def isApproximate(ctr: VarLenCounter): Boolean = ctr match {
    case _: VarLenCounter with Approximate => true
    case _ => false
  }

  /** Abstract hardware counter class
   * 
   * @param sig the signature of the counter
   * 
   * Hardware counters are returned from construction.
   */
  @instantiable
  abstract class HardwareCounter(sig: (Array[Int], Array[Int])) extends Module {
    @public val io = IO(new Bundle {
      val in  = Input(UInt((sig._1.sum).W))
      val out = Output(UInt((sig._2.sum).W))
    })
  }

  /** Abstract counter library trait
   * 
   * Inheriting classes/objects must define two values:
   * - `exactCounters` which is a collection of exact counters
   * - `approxCounters` which is a collection of approximate and exact counters
   * And a function to construct them:
   * - `construct` which takes a counter as argument
   */
  private[Counters] abstract class Library {
    // Collection of exact counters
    val exactCounters: Seq[Counter]

    // Collection of approximate and exact counters
    val approxCounters: Seq[Counter]

    // Collection of exact variable-length counters
    val exactVarLenCounters: Seq[VarLenCounter]

    // Collection of approximate and exact variable-length counters
    val approxVarLenCounters: Seq[VarLenCounter]

    /** Function to construct a counter
     * 
     * @param cntr the counter to construct
     * @param state the present compressor generator state
     * @return a module representing the counter
     */
    def construct(cntr: Counter, state: State): Instance[HardwareCounter]

    /** Function to construct a variable-length counter
     * 
     * @param cntr the variable-length counter to construct
     * @param len the length of the counter to construct
     * @param state the present compressor generator state
     * @return a module representing the counter
     */
    def construct(cntr: VarLenCounter, len: Int, state: State): Instance[HardwareCounter]
  }

  /** Collection of counters for ASIC
   * 
   * The current library includes no atoms and the following standalone counters:
   * - (2 : 1,1]
   * - (3 : 1,1]
   * - (5 : 2,1]
   * - (5 : 2,1] (approximate)
   * - (5 : 1,1] (approximate)
   * - (5 : 2,1] (approximate)
   * - (5 : 1,1,1]
   * - (7 : 1,1,1]
   * - (8 : 1,1,1] (approximate)
   */
  object ASIC extends Library {
    /** Counter (2 : 1,1] (half adder) */
    private[ASIC] class Counter2_11 extends Counter((Array(2), Array(1, 1)), 2) with Exact

    /** Counter (3 : 1,1] (full adder) */
    private[ASIC] class Counter3_11 extends Counter((Array(3), Array(1, 1)), 3) with Exact

    /** Counter (5 : 2,1] */
    private[ASIC] class Counter4_21 extends Counter((Array(4), Array(1, 2)), 6) with Exact

    /** Approximate counter (5 : 2,1] (4:2 compressor)
     * 
     * Implementation of the counter (design 1) from Momeni et al. [2014]
     */
    private[ASIC] class Counter4_21Momeni extends Counter((Array(5), Array(1, 2)), 5) with Approximate

    /** Approximate counter (5 : 1,1] (4:2 compressor)
     * 
     * Implementation of the counter (design 2) from Momeni et al. [2014]
     */
    private[ASIC] class Counter4_11Momeni extends Counter((Array(5), Array(1, 1)), 4) with Approximate

    /** Approximate counter (5 : 2,1] (4:2 compressor)
     * 
     * Implementation of the counter from Moaiyeri et al. [2017]
     */
    private[ASIC] class Counter4_21Moaiyeri extends Counter((Array(5), Array(1, 2)), 4) with Approximate

    /** Counter (5 : 1,1,1] (4:2 compressor) */
    private[ASIC] class Counter5_111 extends Counter((Array(5), Array(1, 1, 1)), 8) with Exact

    /** Counter (7 : 1,1,1] */
    private[ASIC] class Counter7_111 extends Counter((Array(7), Array(1, 1, 1)), 12) with Exact

    /** Approximate counter (8 : 1,1,1]
     * 
     * Implementation of the counter from Boroumand and Brisk [2019]
     */
    private[ASIC] class Counter8_111 extends Counter((Array(8), Array(1, 1, 1)), 11) with Approximate

    /** Collection of exact counters */
    lazy val exactCounters: Seq[Counter] = Seq(
      (new Counter2_11),
      (new Counter3_11),
      (new Counter4_21),
      (new Counter5_111),
      (new Counter7_111)
    )

    /** Collection of approximate and exact counters */
    lazy val approxCounters: Seq[Counter] = exactCounters ++ Seq(
      (new Counter4_21Momeni),
      (new Counter4_11Momeni),
      (new Counter4_21Moaiyeri),
      (new Counter8_111)
    )

    /** Collection of exact variable-length counters */
    lazy val exactVarLenCounters: Seq[VarLenCounter] = Seq()

    /** Collection of approximate and exact variable-length counters */
    lazy val approxVarLenCounters: Seq[VarLenCounter] = exactVarLenCounters ++ Seq()

    /** Function to construct a counter */
    def construct(cntr: Counter, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class ASICCounter(counter: Counter) extends HardwareCounter(counter.sig) {
        /** Depending on the counter, we need to instantiate different amounts
         * of logic here. We wrap existing compressor implementations when possible.
         */
        counter match {
          case _: Counter2_11 =>
            val comp = Module(new Compressor2to2)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            io.out := comp.io.cout ## comp.io.s

          case _: Counter3_11 =>
            val comp = Module(new Compressor3to2)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            comp.io.x3 := io.in(2)
            io.out := comp.io.cout ## comp.io.s

          case _: Counter4_21 =>
            val comp = Module(new Compressor4to2)
            comp.io.x1  := io.in(0)
            comp.io.x2  := io.in(1)
            comp.io.x3  := io.in(2)
            comp.io.x4  := io.in(3)
            comp.io.cin := io.in(4)
            io.out := comp.io.cout ## comp.io.c ## comp.io.s

          case _: Counter4_21Momeni =>
            val comp = Module(new Compressor4to2D1)
            comp.io.x1  := io.in(0)
            comp.io.x2  := io.in(1)
            comp.io.x3  := io.in(2)
            comp.io.x4  := io.in(3)
            comp.io.cin := io.in(4)
            io.out := comp.io.cout ## comp.io.c ## comp.io.s

          case _: Counter4_11Momeni =>
            val comp = Module(new Compressor4to2D2)
            comp.io.x1  := io.in(0)
            comp.io.x2  := io.in(1)
            comp.io.x3  := io.in(2)
            comp.io.x4  := io.in(3)
            comp.io.cin := io.in(4)
            io.out := comp.io.c ## comp.io.s

          case _: Counter4_21Moaiyeri =>
            val comp = Module(new Compressor4to2Maj)
            comp.io.x1  := io.in(0)
            comp.io.x2  := io.in(1)
            comp.io.x3  := io.in(2)
            comp.io.x4  := io.in(3)
            comp.io.cin := io.in(4)
            io.out := comp.io.cout ## comp.io.c ## comp.io.s

          case _: Counter5_111 =>
            val comp = Module(new Compressor5to3)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            comp.io.x3 := io.in(2)
            comp.io.x4 := io.in(3)
            comp.io.x5 := io.in(4)
            io.out := comp.io.c2 ## comp.io.c1 ## comp.io.s

          case _: Counter7_111 =>
            val comp = Module(new Compressor7to3)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            comp.io.x3 := io.in(2)
            comp.io.x4 := io.in(3)
            comp.io.x5 := io.in(4)
            comp.io.x6 := io.in(5)
            comp.io.x7 := io.in(6)
            io.out := comp.io.c2 ## comp.io.c1 ## comp.io.s

          case _: Counter8_111 =>
            val comp = Module(new Compressor8to3)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            comp.io.x3 := io.in(2)
            comp.io.x4 := io.in(3)
            comp.io.x5 := io.in(4)
            comp.io.x6 := io.in(5)
            comp.io.x7 := io.in(6)
            comp.io.x8 := io.in(7)
            io.out := comp.io.z2 ## comp.io.z1 ## comp.io.z0

          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = cntr.getClass().getName()
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new ASICCounter(cntr))
      Instance(state.cntrDefs(cntrName))
    }

    /** Function to construct a variable-length counter */
    def construct(cntr: VarLenCounter, len: Int, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class ASICCounter(counter: VarLenCounter, length: Int) extends HardwareCounter((counter.inSigFn(length), counter.outSigFn(length))) {
        /** Different counters require different amounts of logic here */
        counter match {
          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported variable-length counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = s"${cntr.getClass().getName()}_$len"
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new ASICCounter(cntr, len))
      Instance(state.cntrDefs(cntrName))
    }
  }

  /** Collection of counters for Xilinx 7 Series and UltraScale FPGAs
   * 
   * The current library includes the following atoms:
   * - (2,2)
   * - (1,4)
   * - (0,6)
   * And the following standalone counters:
   * - (2 : 1,1]
   * - (3 : 1,1]
   * - (2,5 : 1,2,1]
   * - (8 : 1,1,1] (approximate)
   */
  object SevenSeries extends Library {
    /** Use an Atom class specific to this library to simplify type checking */
    private[SevenSeries] abstract class SevenSeriesAtom(luts: Int) extends Atom(luts) {
      // Input signature ignoring carries
      def inSig: Array[Int]
    }

    /** Atom (2,2) */
    private[SevenSeries] class Atom22(val inSig: Array[Int] = Array(2, 2)) extends SevenSeriesAtom(2)
    /** Atom (1,4) */
    private[SevenSeries] class Atom14(val inSig: Array[Int] = Array(4, 1)) extends SevenSeriesAtom(2)
    /** Atom (0,6) */
    private[SevenSeries] class Atom06(val inSig: Array[Int] = Array(6, 0)) extends SevenSeriesAtom(2)

    /** Counter (2 : 1,1] (half adder) */
    private[SevenSeries] class Counter2_11 extends Counter((Array(2), Array(1, 1)), 1) with Exact

    /** Counter (3 : 1,1] (full adder) */
    private[SevenSeries] class Counter3_11 extends Counter((Array(3), Array(1, 1)), 1) with Exact

    /** Counter (2,5 : 1,2,1]
     * 
     * Implementation of the counter from Preusser [2017]
     */
    private[SevenSeries] class Counter25_121 extends Counter((Array(5, 2), Array(1, 2, 1)), 2) with Exact

    /** Approximate counter (8 : 1,1,1]
     * 
     * Implementation of the counter from Boroumand and Brisk [2019]
     */
    private[SevenSeries] class Counter8_111 extends Counter((Array(8), Array(1, 1, 1)), 4) with Approximate

    /** Function to compute the signature for a composed counter
     * 
     * @param atom1 the first atom
     * @param atom2 the second atom
     * @return the signature arising from the two atoms' composition
     */
    private[SevenSeries] def compose(atom1: SevenSeriesAtom, atom2: SevenSeriesAtom): (Array[Int], Array[Int]) = {
      val ins = {
        val comb = atom1.inSig ++ atom2.inSig
        comb(0) += 1
        comb
      }
      val outs = Array(1, 1, 1, 1, 1)
      (ins, outs)
    }

    /** Counter composed from two atoms
     * 
     * @param atom1 the first atom
     * @param atom2 the second atom
     */
    private[SevenSeries] class ComposedCounter(val atom1: SevenSeriesAtom, val atom2: SevenSeriesAtom)
      extends Counter(compose(atom1, atom2), atom1.luts + atom2.luts) with Exact

    /** Collection of exact counters */
    lazy val exactCounters: Seq[Counter] = Seq(
      (new Counter2_11),
      (new Counter3_11),
      (new Counter25_121),
      (new ComposedCounter(new Atom22, new Atom22)),
      (new ComposedCounter(new Atom22, new Atom14)),
      (new ComposedCounter(new Atom22, new Atom06)),
      (new ComposedCounter(new Atom14, new Atom22)),
      (new ComposedCounter(new Atom14, new Atom14)),
      (new ComposedCounter(new Atom14, new Atom06)),
      (new ComposedCounter(new Atom06, new Atom22)),
      (new ComposedCounter(new Atom06, new Atom14)),
      (new ComposedCounter(new Atom06, new Atom06))
    )

    /** Collection of approximate and exact counters */
    lazy val approxCounters: Seq[Counter] = exactCounters ++ Seq(
      (new Counter8_111)
    )

    /** Collection of exact variable-length counters */
    lazy val exactVarLenCounters: Seq[VarLenCounter] = Seq()

    /** Collection of approximate and exact variable-length counters */
    lazy val approxVarLenCounters: Seq[VarLenCounter] = exactVarLenCounters ++ Seq()

    /** Function to construct a counter */
    def construct(cntr: Counter, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class SevenSeriesCounter(counter: Counter) extends HardwareCounter(counter.sig) {
        /** Depending on the counter, we need to instantiate different amounts
         * of logic here. Compound counters need CARRY4 primitives.
         */
        counter match {
          case _: Counter2_11 =>
            // Boolean functions for the LUT
            val lutFO5 = (ins: Seq[Boolean]) => ins(0) && ins(1)
            val lutFO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1)

            // LUT computes C0 as O5 and S0 as O6
            // Inputs: (a0 = in(0), a1 = in(1), false, false, false, true)
            val lut = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
            lut.io.I0 := io.in(0)
            lut.io.I1 := io.in(1)
            lut.io.I2 := false.B
            lut.io.I3 := false.B
            lut.io.I4 := false.B
            lut.io.I5 := true.B

            // Outputs: [c0 = out(1), s0 = out(0)]
            io.out := lut.io.O5 ## lut.io.O6

          case _: Counter3_11 =>
            // Boolean functions for the LUT
            val lutFO5 = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
            val lutFO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2)

            // LUT computes C0 as O5 and S0 as O6
            // Inputs: (a0 = in(0), a1 = in(1), a2 = in(2), false, false, true)
            val lut = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
            lut.io.I0 := io.in(0)
            lut.io.I1 := io.in(1)
            lut.io.I2 := io.in(2)
            lut.io.I3 := false.B
            lut.io.I4 := false.B
            lut.io.I5 := true.B

            // Outputs: [c0 = out(1), s0 = out(0)]
            io.out := lut.io.O5 ## lut.io.O6

          case _: Counter25_121 =>
            // Boolean functions for the two LUTs
            val lutLOFO5 = (ins: Seq[Boolean]) => {
              val sint = ins(2) ^ ins(3) ^ ins(4)
              (ins(0) & ins(1)) | (ins(0) & sint) | (ins(1) & sint)
            }
            val lutLOFO6 = (ins: Seq[Boolean]) => {
              val sint = ins(2) ^ ins(3) ^ ins(4)
              ins(0) ^ ins(1) ^ sint
            }
            val lutHIFO5 = (ins: Seq[Boolean]) => {
              val cint = (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
              (cint & ins(3)) | (cint & ins(4)) | (ins(3) & ins(4))
            }
            val lutHIFO6 = (ins: Seq[Boolean]) => {
              val cint = (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
              cint ^ ins(3) ^ ins(4)
            }

            // LUT LO computes C0 as O5 and S0 as O6
            // Inputs: (a0 = in(0), a1 = in(1), a2 = in(2), a3 = in(3), a4 = in(4), false)
            val lutLO = Module(new LUT6_2(genLUT6_2InitString(lutLOFO5, lutLOFO6)))
            lutLO.io.I0 := io.in(0)
            lutLO.io.I1 := io.in(1)
            lutLO.io.I2 := io.in(2)
            lutLO.io.I3 := io.in(3)
            lutLO.io.I4 := io.in(3)
            lutLO.io.I5 := false.B

            // LUT HI computes C1 as O5 and S1 as O6
            // Inputs: (a2 = in(2), a3 = in(3), a4 = in(4), b0 = in(5), b1 = in(6), false)
            val lutHI = Module(new LUT6_2(genLUT6_2InitString(lutHIFO5, lutHIFO6)))
            lutHI.io.I0 := io.in(2)
            lutHI.io.I1 := io.in(3)
            lutHI.io.I2 := io.in(4)
            lutHI.io.I3 := io.in(5)
            lutHI.io.I4 := io.in(6)
            lutHI.io.I5 := false.B

            // Outputs: [c1 = out(3), s1 = out(2), c0 = out(1), s0 = out(0)]
            io.out := lutHI.io.O5 ## lutHI.io.O6 ## lutLO.io.O5 ## lutLO.io.O6

          case _: Counter8_111 =>
            // Boolean functions for the three LUTs
            val lutS0FO5 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2)
            val lutS0FO6 = (ins: Seq[Boolean]) => false
            val lutS1FO5 = (ins: Seq[Boolean]) => {
              val or = ins(2) || ins(3)
              (ins(0) && ins(1)) || (ins(0) && or) || (ins(1) && or)
            }
            val lutS1FO6 = (ins: Seq[Boolean]) => {
              val or = ins(2) || ins(3)
              val s1 = ins(0) ^ ins(1) ^ or
              ins(4) ^ s1
            }
            val lutC0FO5 = (ins: Seq[Boolean]) => false
            val lutC0FO6 = (ins: Seq[Boolean]) => {
              val c0 = (ins(1) && ins(2)) || (ins(1) && ins(3)) || (ins(2) && ins(3))
              ins(0) ^ c0
            }

            // The design uses a CARRY4 and two general-purpose routing paths
            val s0 = Wire(Bool())
            val c1 = Wire(Bool())
            val carry = Module(new CARRY4)

            // LUT S0 computes S0 as O5 and false as O6
            // Inputs: (x2 = in(1), x3 = in(2), x4 = in(3), false, false, true)
            val lutS0 = Module(new LUT6_2(genLUT6_2InitString(lutS0FO5, lutS0FO6)))
            lutS0.io.I0 := io.in(1)
            lutS0.io.I1 := io.in(2)
            lutS0.io.I2 := io.in(3)
            lutS0.io.I3 := false.B
            lutS0.io.I4 := false.B
            lutS0.io.I5 := true.B
            s0 := lutS0.io.O5

            // LUT S1 computes C1 as O5 and S1 xor S0 as O6
            // Inputs: (x5 = in(4), x6 = in(5), x7 = in(6), x8 = in(7), s0, true)
            val lutS1 = Module(new LUT6_2(genLUT6_2InitString(lutS1FO5, lutS1FO6)))
            lutS1.io.I0 := io.in(4)
            lutS1.io.I1 := io.in(5)
            lutS1.io.I2 := io.in(6)
            lutS1.io.I3 := io.in(7)
            lutS1.io.I4 := s0
            lutS1.io.I5 := true.B
            c1 := lutS1.io.O5

            // LUT C0 computes nothing as O5 and C1 xor C0 as O6
            // Inputs: (c1, x2 = in(1), x3 = in(2), x4 = in(3), false, true)
            val lutC0 = Module(new LUT6_2(genLUT6_2InitString(lutC0FO5, lutC0FO6)))
            lutC0.io.I0 := c1
            lutC0.io.I1 := io.in(1)
            lutC0.io.I2 := io.in(2)
            lutC0.io.I3 := io.in(3)
            lutC0.io.I4 := false.B
            lutC0.io.I5 := true.B

            // The CARRY4 computes the sum bits
            carry.io.CYINIT := false.B
            carry.io.CI     := false.B
            carry.io.DI     := false.B ## c1 ## s0 ## io.in(0)
            carry.io.S      := false.B ## lutC0.io.O6 ## lutS1.io.O6 ## lutS0.io.O6

            // Outputs: [z2 = out(2), z1 = out(1), z0 = out(0)]
            io.out := carry.io.O(3) ## carry.io.O(2) ## carry.io.O(1)

          case comp: ComposedCounter =>
            /** Instantiate a CARRY4 block and connect the two atoms to it */

            /** Build the LUT structure of an atom (2,2)
             * 
             * @param inputs the input bits to the structure
             * @return a tuple of (O6s, O5s) output bits
             * 
             * Implementation of the atom from Preusser [2017]
             */
            def buildAtom22(inputs: UInt): (UInt, UInt) = {
              // Boolean functions for the two LUTs
              val lutFO5 = (ins: Seq[Boolean]) => ins(1)
              val lutFO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1)

              // Inputs: (a0 = inputs(0), a1 = inputs(1), false, false, false, false)
              val lut0 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
              lut0.io.I0 := inputs(0)
              lut0.io.I1 := inputs(1)
              lut0.io.I2 := false.B
              lut0.io.I3 := false.B
              lut0.io.I4 := false.B
              lut0.io.I5 := false.B

              // Inputs: (b0 = inputs(2), b1 = inputs(3), false, false, false, false)
              val lut1 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
              lut1.io.I0 := inputs(2)
              lut1.io.I1 := inputs(3)
              lut1.io.I2 := false.B
              lut1.io.I3 := false.B
              lut1.io.I4 := false.B
              lut1.io.I5 := false.B

              // Outputs combined
              (lut1.io.O6 ## lut0.io.O6, lut1.io.O5 ## lut0.io.O5)
            }

            /** Build the LUT structure of an atom (1,4)
             * 
             * @param inputs the input bits to the structure
             * @return a tuple of (O6s, O5s) output bits
             * 
             * Implementation of the atom from Preusser [2017]
             */
            def buildAtom14(inputs: UInt): (UInt, UInt) = {
              // Boolean functions for the two LUTs
              val lutFO5  = (ins: Seq[Boolean]) => ins(3)
              val lut0FO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2) ^ ins(3)
              val lut1FO6 = (ins: Seq[Boolean]) => ((ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))) ^ ins(3)

              // Inputs: (a0 = inputs(0), a1 = inputs(1), a2 = inputs(2), a3 = inputs(3), false, false)
              val lut0 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lut0FO6)))
              lut0.io.I0 := inputs(0)
              lut0.io.I1 := inputs(1)
              lut0.io.I2 := inputs(2)
              lut0.io.I3 := inputs(3)
              lut0.io.I4 := false.B
              lut0.io.I5 := false.B

              // Inputs: (a0 = inputs(0), a1 = inputs(1), a2 = inputs(2), b1 = inputs(4), false, false)
              val lut1 = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lut1FO6)))
              lut1.io.I0 := inputs(0)
              lut1.io.I1 := inputs(1)
              lut1.io.I2 := inputs(2)
              lut1.io.I3 := inputs(4)
              lut1.io.I4 := false.B
              lut1.io.I5 := false.B

              // Outputs combined
              (lut1.io.O6 ## lut0.io.O6, lut1.io.O5 ## lut0.io.O5)
            }

            /** Build the LUT structure of an atom (0,6)
             * 
             * @param inputs the input bits to the structure
             * @return a tuple of (O6s, O5s) output bits
             * 
             * Implementation of the atom from Preusser [2017]
             */
            def buildAtom06(inputs: UInt): (UInt, UInt) = {
              // Boolean functions for the two LUTs
              val lut0FO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2) ^ ins(3) ^ ins(4) ^ ins(5)
              val lut1FO5 = (ins: Seq[Boolean]) => (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
              val lut1FO6 = (ins: Seq[Boolean]) => {
                val s0 = ins(0) ^ ins(1) ^ ins(2)
                val c0 = (ins(0) & ins(1)) | (ins(0) & ins(2)) | (ins(1) & ins(2))
                val c1 = (s0 & ins(3)) | (s0 & ins(4)) | (ins(3) & ins(4))
                c0 ^ c1
              }

              // Inputs: (a0 = inputs(0), a1 = inputs(1), a2 = inputs(2), a3 = inputs(3), a4 = inputs(4), a5 = inputs(5))
              val lut0 = Module(new LUT6(genLUT6InitString(lut0FO6)))
              lut0.io.I0 := inputs(0)
              lut0.io.I1 := inputs(1)
              lut0.io.I2 := inputs(2)
              lut0.io.I3 := inputs(3)
              lut0.io.I4 := inputs(4)
              lut0.io.I5 := inputs(5)

              // Inputs: (a0 = inputs(0), a1 = inputs(1), a2 = inputs(2), a3 = inputs(3), a4 = inputs(4), false)
              val lut1 = Module(new LUT6_2(genLUT6_2InitString(lut1FO5, lut1FO6)))
              lut1.io.I0 := inputs(0)
              lut1.io.I1 := inputs(1)
              lut1.io.I2 := inputs(2)
              lut1.io.I3 := inputs(3)
              lut1.io.I4 := inputs(4)
              lut1.io.I5 := false.B

              // Outputs combined
              (lut1.io.O6 ## lut0.io.O, lut1.io.O5 ## inputs(5))
            }

            // Split the inputs between the two atoms and the CARRY4 block
            val ins = Wire(MixedVec(Bool(), UInt(comp.atom1.inSig.sum.W), UInt(comp.atom2.inSig.sum.W)))
            ins := io.in.asTypeOf(ins)

            // Generate the LUT structure of the two atoms and connect their inputs
            val atom1LUTs = comp.atom1 match {
              case _: Atom22 => buildAtom22(ins(1))
              case _: Atom14 => buildAtom14(ins(1))
              case _: Atom06 => buildAtom06(ins(1))
              case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported atom ${comp.atom1}")
            }
            val atom2LUTs = comp.atom2 match {
              case _: Atom22 => buildAtom22(ins(2))
              case _: Atom14 => buildAtom14(ins(2))
              case _: Atom06 => buildAtom06(ins(2))
              case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported atom ${comp.atom2}")
            }

            // Connect the LUT structures' outputs to a CARRY4 block
            val carry = Module(new CARRY4)
            carry.io.CYINIT := false.B
            carry.io.CI     := ins(0)
            carry.io.DI     := atom2LUTs._2 ## atom1LUTs._2
            carry.io.S      := atom2LUTs._1 ## atom1LUTs._1

            // Outputs: [c3 = out(4), s3 = out(3), s2 = out(2), s1 = out(1), s0 = out(0)]
            io.out := carry.io.CO(3) ## carry.io.O

          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = cntr.getClass().getName()
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new SevenSeriesCounter(cntr))
      Instance(state.cntrDefs(cntrName))
    }

    /** Function to construct a variable-length counter */
    def construct(cntr: VarLenCounter, len: Int, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class SevenSeriesCounter(counter: VarLenCounter, length: Int) extends HardwareCounter((counter.inSigFn(length), counter.outSigFn(length))) {
        /** Different counters require different amounts of logic here */
        counter match {
          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported variable-length counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = s"${cntr.getClass().getName()}_$len"
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new SevenSeriesCounter(cntr, len))
      Instance(state.cntrDefs(cntrName))
    }
  }

  /** Collection of counters for Xilinx Versal FPGAs
   * 
   * The current library includes the following atoms:
   * - (2,2)
   * - (1,4)
   * And the following standalone counters:
   * - (2 : 1,1]
   * - (3 : 1,1]
   * - (2,5 : 1,2,1]
   * - (7 : 1,1,1]
   * - (8 : 1,1,1] (approximate)
   * - (10 : 4,2]
   */
  object Versal extends Library {
    /** Use an Atom class specific to this library to simplify type checking */
    private[Versal] abstract class VersalAtom(luts: Int) extends Atom(luts) {
      // Input signature ignoring carries
      def inSig: Array[Int]
    }

    /** Atom (2,2) */
    private[Versal] class Atom22(val inSig: Array[Int] = Array(2, 2)) extends VersalAtom(2)
    /** Atom (1,4) */
    private[Versal] class Atom14(val inSig: Array[Int] = Array(4, 1)) extends VersalAtom(2)

    /** Counter (2 : 1,1] (half adder) */
    private[Versal] class Counter2_11 extends Counter((Array(2), Array(1, 1)), 1) with Exact

    /** Counter (3 : 1,1] (full adder) */
    private[Versal] class Counter3_11 extends Counter((Array(3), Array(1, 1)), 1) with Exact

    /** Counter (2,5 : 1,2,1]
     * 
     * Adaptation of the counter from Preusser [2017]
     */
    private[Versal] class Counter25_121 extends Counter((Array(5, 2), Array(1, 2, 1)), 2) with Exact

    /** Counter (7 : 1,1,1] */
    private[Versal] class Counter7_111 extends Counter((Array(7), Array(1, 1, 1)), 3) with Exact

    /** Counter (10 : 4,2]
     * 
     * Adaptation of the counter from Hossfeld et al. [2024]
     */
    private[Versal] class Counter10_42 extends Counter((Array(10), Array(4, 2)), 3) with Exact

    /** Approximate counter (8 : 1,1,1]
     * 
     * Adaptation of the counter from Boroumand and Brisk [2019]
     */
    private[Versal] class Counter8_111 extends Counter((Array(8), Array(1, 1, 1)), 3) with Approximate

    /** Function to compute the signature for a composed counter
     * 
     * @param atoms a list of atoms to compose
     * @return the signature arising from the atoms' composition
     */
    private[Versal] def compose(atoms: Seq[VersalAtom]): (Array[Int], Array[Int]) = {
      require(!atoms.isEmpty && atoms.length <= 4, "can only compose between 1 and 4 atoms")
      val ins  = atoms.flatMap(_.inSig).toArray
      ins(0)  += 1
      val outs = Array.fill(2*atoms.length + 1)(1)
      (ins, outs)
    }

    /** Counter composed from two atoms
     * 
     * @param atoms a list of atoms to compose
     */
    private[Versal] class ComposedCounter(val atoms: Seq[VersalAtom])
      extends Counter(compose(atoms), atoms.map(_.luts).sum) with Exact

    /** Ripple-sum counter with signature (2n+1 : n,1]
     * 
     * Adaptation of the counter from Hossfeld et al. [2024]
     */
    private[Versal] class RippleSum extends VarLenCounter(
      (n: Int) => Array(1, n),
      (n: Int) => Array(2*n + 1),
      (n: Int) => n
    ) with Exact

    /** Dual-rail ripple-sum counter with signature (n+1,4n+1 : n,n+1,1]
     * 
     * Adaptation of the counter from Hossfeld et al. [2024]
     */
    private[Versal] class DualRailRippleSum extends VarLenCounter(
      (n: Int) => Array(4*n + 1, n + 1),
      (n: Int) => Array(1, n + 1, n),
      (n: Int) => 2*n
    ) with Exact

    /** Collection of exact counters */
    lazy val exactCounters: Seq[Counter] = Seq(
      (new Counter2_11),
      (new Counter3_11),
      (new Counter25_121),
      (new Counter7_111),
      (new Counter10_42)
    ) ++ (Seq.fill(4) { new Atom22 } ++ Seq.fill(4) { new Atom14 }).combinations(4).map(atoms => new ComposedCounter(atoms))

    /** Collection of approximate and exact counters */
    lazy val approxCounters: Seq[Counter] = exactCounters ++ Seq(
      (new Counter8_111)
    )

    /** Collection of exact variable-length counters */
    lazy val exactVarLenCounters: Seq[VarLenCounter] = Seq(
      (new RippleSum),
      (new DualRailRippleSum)
    )

    /** Collection of approximate and exact variable-length counters */
    lazy val approxVarLenCounters: Seq[VarLenCounter] = exactVarLenCounters ++ Seq()

    /** Function to construct a counter */
    def construct(cntr: Counter, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class VersalCounter(counter: Counter) extends HardwareCounter(counter.sig) {
        /** Depending on the counter, we need to instantiate different amounts
         * of logic here. Compound counters need LOOKAHEAD8 primitives.
         */
        counter match {
          case _: Counter2_11 =>
            // Boolean functions for the LUT
            val lutFO51 = (ins: Seq[Boolean]) => ins(0) && ins(1)
            val lutFO52 = (ins: Seq[Boolean]) => ins(0) ^ ins(1)

            // LUT computes C0 as O51 and S0 as O52
            // Inputs: (a0 = in(0), a1 = in(1), false, false, false)
            val lut = Module(new LUT6CY(genLUT6CYInitString(lutFO51, lutFO52)))
            lut.io.I0 := io.in(0)
            lut.io.I1 := io.in(1)
            lut.io.I2 := false.B
            lut.io.I3 := false.B
            lut.io.I4 := false.B

            // Outputs: [c0 = out(1), s0 = out(0)]
            io.out := lut.io.O51 ## lut.io.O52

          case _: Counter3_11 =>
            // Boolean functions for the LUT
            val lutFO51 = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
            val lutFO52 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(2)

            // LUT computes C0 as O51 and S0 as O52
            // Inputs: (a0 = in(0), a1 = in(1), a2 = in(2), false, false)
            val lut = Module(new LUT6CY(genLUT6CYInitString(lutFO51, lutFO52)))
            lut.io.I0 := io.in(0)
            lut.io.I1 := io.in(1)
            lut.io.I2 := io.in(2)
            lut.io.I3 := false.B
            lut.io.I4 := false.B

            // Outputs: [c0 = out(1), s0 = out(0)]
            io.out := lut.io.O51 ## lut.io.O52

          case _: Counter25_121 =>
            // Boolean functions for the two LUTs
            val lutLOFO51 = (ins: Seq[Boolean]) => ins.take(5).reduceLeft(_ ^ _)
            val lutLOFO52 = (ins: Seq[Boolean]) => {
              val s2 = ins(2) ^ ins(3) ^ ins(4)
              (ins(0) && s2) || (ins(1) && s2) || (ins(0) && ins(1))
            }
            val lutHIFO51 = (ins: Seq[Boolean]) => {
              val c0 = (ins(2) && ins(3)) || (ins(2) && ins(4)) || (ins(3) && ins(4))
              ins(0) ^ ins(1) ^ c0
            }
            val lutHIFO52 = (ins: Seq[Boolean]) => {
              val c0 = (ins(2) && ins(3)) || (ins(2) && ins(4)) || (ins(3) && ins(4))
              (ins(0) && ins(1)) || (ins(0) && c0) || (ins(1) && c0)
            }

            // LUTHI computes s0 as O51 and c0 as O52
            // Inputs: (x0 = in(0), x1 = in(1), x2 = in(2), x3 = in(3), x4 = in(4))
            val lutLO = Module(new LUT6CY(genLUT6CYInitString(lutLOFO51, lutLOFO52)))
            lutLO.io.I0 := io.in(0)
            lutLO.io.I1 := io.in(1)
            lutLO.io.I2 := io.in(2)
            lutLO.io.I3 := io.in(3)
            lutLO.io.I4 := io.in(4)

            // LUTLO computes c1 as O51 and t1' as O52
            // Inputs: (x5 = in(0), x6 = in(1), x2 = in(2), x3 = in(3), x4 = in(4))
            val lutHI = Module(new LUT6CY(genLUT6CYInitString(lutHIFO51, lutHIFO52)))
            lutHI.io.I0 := io.in(5)
            lutHI.io.I1 := io.in(6)
            lutHI.io.I2 := io.in(2)
            lutHI.io.I3 := io.in(3)
            lutHI.io.I4 := io.in(4)

            // Outputs: [t1' = out(3), c1 = out(2), c0 = out(1), s0 = out(0)]
            io.out := lutHI.io.O52 ## lutHI.io.O51 ## lutLO.io.O52 ## lutLO.io.O51

          case _: Counter7_111 =>
            // Boolean functions for the three LUTs
            val lutS1FO51 = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
            val lutS1FO52 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ (ins(2) || ins(3))
            val lutC2FO51 = (ins: Seq[Boolean]) => {
              val s0 = ins(1) ^ ins(2) ^ ins(3)
              ins(0) ^ s0 ^ ins(4)
            }
            val lutC2FO52 = (ins: Seq[Boolean]) => {
              val s0 = ins(1) ^ ins(2) ^ ins(3)
              (ins(0) && s0) || (ins(0) && ins(4)) || (s0 && ins(4))
            }
            val lutZFO51  = (ins: Seq[Boolean]) => {
              val c0 = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
              c0 ^ ins(3) ^ ins(4)
            }
            val lutZFO52  = (ins: Seq[Boolean]) => {
              val c0 = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
              (c0 && ins(3)) || (c0 && ins(4)) || (ins(3) && ins(4))
            }

            // The design uses two cascades and one general-purpose routing path
            val c1 = Wire(Bool())
            val s1 = Wire(Bool())
            val c2 = Wire(Bool())

            // LUT S1 computes C1 as O51 and S1 as O52
            // Inputs: (x5 = in(4), x6 = in(5), x7 = in(6), false, false)
            val lutS1 = Module(new LUT6CY(genLUT6CYInitString(lutS1FO51, lutS1FO52)))
            lutS1.io.I0 := io.in(4)
            lutS1.io.I1 := io.in(5)
            lutS1.io.I2 := io.in(6)
            lutS1.io.I3 := false.B
            lutS1.io.I4 := false.B
            c1 := lutS1.io.O51
            s1 := lutS1.io.O52

            // LUT C2 computes Z0 and O51 and C2 as O52
            // Inputs: (x1 = in(0), x2 = in(1), x3 = in(2), x4 = in(3), s1)
            val lutC2 = Module(new LUT6CY(genLUT6CYInitString(lutC2FO51, lutC2FO52)))
            lutC2.io.I0 := io.in(0)
            lutC2.io.I1 := io.in(1)
            lutC2.io.I2 := io.in(2)
            lutC2.io.I3 := io.in(3)
            lutC2.io.I4 := s1
            c2    := lutC2.io.O52

            // LUT Z computes Z1 as O51 and Z2 as O52
            // Inputs: (x2 = in(1), x3 = in(2), x4 = in(3), c1, c2)
            val lutZ = Module(new LUT6CY(genLUT6CYInitString(lutZFO51, lutZFO52)))
            lutZ.io.I0 := io.in(1)
            lutZ.io.I1 := io.in(2)
            lutZ.io.I2 := io.in(3)
            lutZ.io.I3 := c1
            lutZ.io.I4 := c2

            // Outputs: [z2 = out(2), z1 = out(1), z0 = out(0)]
            io.out := lutZ.io.O52 ## lutZ.io.O51 ## lutC2.io.O51

          case _: Counter10_42 =>
            // Boolean functions for the three LUTs
            val lut0FO51 = (ins: Seq[Boolean]) => ins.take(5).reduceLeft(_ ^ _)
            val lut0FO52 = (ins: Seq[Boolean]) => {
              val s2 = ins(2) ^ ins(3) ^ ins(4)
              (ins(0) && s2) || (ins(1) && s2) || (ins(0) && ins(1))
            }
            val lut1FO51 = lut0FO51
            val lut1FO52 = lut0FO52
            val lut2FO5  = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
            val lut2FO6  = (ins: Seq[Boolean]) => (ins(3) && ins(4)) || (ins(3) && ins(5)) || (ins(4) && ins(5))

            // LUT0 computes S0 as O51 and C0 as O52
            // Inputs: (x0 = in(0), x1 = in(1), x2 = in(2), x3 = in(3), x4 = in(4))
            val lut0 = Module(new LUT6CY(genLUT6CYInitString(lut0FO51, lut0FO52)))
            lut0.io.I0 := io.in(0)
            lut0.io.I1 := io.in(1)
            lut0.io.I2 := io.in(2)
            lut0.io.I3 := io.in(3)
            lut0.io.I4 := io.in(4)

            // LUT1 computes S1 as O51 and C1 as O52
            // Inputs: (x5 = in(5), x6 = in(6), x7 = in(7), x8 = in(8), x9 = in(9))
            val lut1 = Module(new LUT6CY(genLUT6CYInitString(lut1FO51, lut1FO52)))
            lut1.io.I0 := io.in(5)
            lut1.io.I1 := io.in(6)
            lut1.io.I2 := io.in(7)
            lut1.io.I3 := io.in(8)
            lut1.io.I4 := io.in(9)

            // LUT2 computes C2 and C3
            // Inputs: (x2 = in(2), x3 = in(3), x4 = in(4), x7 = in(7), x8 = in(8), x9 = in(9))
            val lut2 = Module(new LUT6_2(genLUT6_2InitString(lut2FO5, lut2FO6)))
            lut2.io.I0 := io.in(2)
            lut2.io.I1 := io.in(3)
            lut2.io.I2 := io.in(4)
            lut2.io.I3 := io.in(7)
            lut2.io.I4 := io.in(8)
            lut2.io.I5 := io.in(9)

            // Outputs: [t3 = out(5), t2 = out(4), t1 = out(3), t0 = out(2), s1 = out(1), s0 = out(0)]
            io.out := lut2.io.O6 ## lut2.io.O5 ## lut1.io.O52 ## lut0.io.O52 ## lut1.io.O51 ## lut0.io.O51

          case _: Counter8_111 =>
            // Boolean functions for the three LUTs
            val lutS1FO51 = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && (ins(2) || ins(3))) || (ins(1) && (ins(2) || ins(3)))
            val lutS1FO52 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ (ins(2) || ins(3))
            val lutC2FO51 = (ins: Seq[Boolean]) => {
              val s0 = ins(1) ^ ins(2) ^ ins(3)
              ins(0) ^ s0 ^ ins(4)
            }
            val lutC2FO52 = (ins: Seq[Boolean]) => {
              val s0 = ins(1) ^ ins(2) ^ ins(3)
              (ins(0) && s0) || (ins(0) && ins(4)) || (s0 && ins(4))
            }
            val lutZFO51  = (ins: Seq[Boolean]) => {
              val c0 = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
              c0 ^ ins(3) ^ ins(4)
            }
            val lutZFO52  = (ins: Seq[Boolean]) => {
              val c0 = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
              (c0 && ins(3)) || (c0 && ins(4)) || (ins(3) && ins(4))
            }

            // The design uses two cascades and one general-purpose routing path
            val c1 = Wire(Bool())
            val s1 = Wire(Bool())
            val c2 = Wire(Bool())

            // LUT S1 computes C1 as O51 and S1 as O52
            // Inputs: (x5 = in(4), x6 = in(5), x7 = in(6), x8 = in(7), false)
            val lutS1 = Module(new LUT6CY(genLUT6CYInitString(lutS1FO51, lutS1FO52)))
            lutS1.io.I0 := io.in(4)
            lutS1.io.I1 := io.in(5)
            lutS1.io.I2 := io.in(6)
            lutS1.io.I3 := io.in(7)
            lutS1.io.I4 := false.B
            c1 := lutS1.io.O51
            s1 := lutS1.io.O52

            // LUT C2 computes Z0 and O51 and C2 as O52
            // Inputs: (x1 = in(0), x2 = in(1), x3 = in(2), x4 = in(3), s1)
            val lutC2 = Module(new LUT6CY(genLUT6CYInitString(lutC2FO51, lutC2FO52)))
            lutC2.io.I0 := io.in(0)
            lutC2.io.I1 := io.in(1)
            lutC2.io.I2 := io.in(2)
            lutC2.io.I3 := io.in(3)
            lutC2.io.I4 := s1
            c2    := lutC2.io.O52

            // LUT Z computes Z1 as O51 and Z2 as O52
            // Inputs: (x2 = in(1), x3 = in(2), x4 = in(3), c1, c2)
            val lutZ = Module(new LUT6CY(genLUT6CYInitString(lutZFO51, lutZFO52)))
            lutZ.io.I0 := io.in(1)
            lutZ.io.I1 := io.in(2)
            lutZ.io.I2 := io.in(3)
            lutZ.io.I3 := c1
            lutZ.io.I4 := c2

            // Outputs: [z2 = out(2), z1 = out(1), z0 = out(0)]
            io.out := lutZ.io.O52 ## lutZ.io.O51 ## lutC2.io.O51

          case comp: ComposedCounter =>
            /** Instantiate a LOOKAHEAD8 block and connect the atoms to it */

            /** Build the LUT structure of an atom (2,2)
             * 
             * @param inputs the input bits to the structure
             * @param cins the input carries to the structure
             * @return a triple of (O51s, O52s, PROPs) output bits
             * 
             * Implementation of the atom from Hossfeld et al. [2024]
             */
            def buildAtom22(inputs: UInt, cins: UInt): (UInt, UInt, UInt) = {
              // Boolean functions for the LUT
              val lutFO51 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(4)
              val lutFO52 = (ins: Seq[Boolean]) => {
                (ins(0) && ins(1)) || (ins(0) && ins(4)) || (ins(1) && ins(4))
              }

              // Inputs: (a0 = inputs(0), a1 = inputs(1), false, false, cins(0), false)
              val lut0 = Module(new LUT6CY(genLUT6CYInitString(lutFO51, lutFO52)))
              lut0.io.I0 := inputs(0)
              lut0.io.I1 := inputs(1)
              lut0.io.I2 := false.B
              lut0.io.I3 := false.B
              lut0.io.I4 := cins(0)

              // Inputs: (b0 = inputs(2), b1 = inputs(3), false, false, cins(1), false)
              val lut1 = Module(new LUT6CY(genLUT6CYInitString(lutFO51, lutFO52)))
              lut1.io.I0 := inputs(2)
              lut1.io.I1 := inputs(3)
              lut1.io.I2 := false.B
              lut1.io.I3 := false.B
              lut1.io.I4 := cins(1)

              // Outputs combined
              (lut1.io.O51 ## lut0.io.O51, lut1.io.O52 ## lut0.io.O52, lut1.io.PROP ## lut0.io.PROP)
            }

            /** Build the LUT structure of an atom (1,4)
             * 
             * @param inputs the input bits to the structure
             * @param cins the input carries to the structure
             * @return a triple of (O51s, O52s, PROPs) output bits
             * 
             * Implementation of the atom from Hossfeld et al. [2024]
             */
            def buildAtom14(inputs: UInt, cins: UInt): (UInt, UInt, UInt) = {
              // Boolean functions for the two LUTs
              val lut0FO51 = (ins: Seq[Boolean]) => ins.take(5).reduce(_ ^ _)
              val lut0FO52 = (ins: Seq[Boolean]) => {
                val s = ins.take(3).reduce(_ ^ _)
                (s && ins(3)) || (s && ins(4)) || (ins(3) && ins(4))
              }
              val lut1FO51 = (ins: Seq[Boolean]) => {
                val c = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
                c ^ ins(3) ^ ins(4)
              }
              val lut1FO52 = (ins: Seq[Boolean]) => {
                val c = (ins(0) && ins(1)) || (ins(0) && ins(2)) || (ins(1) && ins(2))
                (c && ins(3)) || (c && ins(4)) || (ins(3) && ins(4))
              }

              // Inputs: (a0 = inputs(0), a1 = inputs(1), a2 = inputs(2), a3 = inputs(3), a4 = cins(0), false)
              val lut0 = Module(new LUT6CY(genLUT6CYInitString(lut0FO51, lut0FO52)))
              lut0.io.I0 := inputs(0)
              lut0.io.I1 := inputs(1)
              lut0.io.I2 := inputs(2)
              lut0.io.I3 := inputs(3)
              lut0.io.I4 := cins(0)

              // Inputs: (a0 = inputs(0), a1 = inputs(1), a2 = inputs(2), a3 = inputs(4), a4 = cins(1), false)
              val lut1 = Module(new LUT6CY(genLUT6CYInitString(lut1FO51, lut1FO52)))
              lut1.io.I0 := inputs(0)
              lut1.io.I1 := inputs(1)
              lut1.io.I2 := inputs(2)
              lut1.io.I3 := inputs(4)
              lut1.io.I4 := cins(1)

              // Outputs combined
              (lut1.io.O51 ## lut0.io.O51, lut1.io.O52 ## lut0.io.O52, lut1.io.PROP ## lut0.io.PROP)
            }

            // Split the inputs between the atoms and the LOOKAHEAD8 block
            val ins = Wire(MixedVec(Bool(), comp.atoms.map { atom => UInt(atom.inSig.sum.W) } :_*))
            ins := io.in.asTypeOf(ins)

            // Instantiate a LOOKAHEAD8 block and get its carry and propagate in-/outputs
            val look8 = Module(new LOOKAHEAD8("TRUE", "TRUE", "TRUE", "TRUE"))
            val look8CYs   = look8.allCYs
            val look8COs   = look8.allCOs
            val look8Props = look8.allProps
            // Default assignments to avoid unconnected wires
            look8CYs  .foreach(_ := false.B)
            look8Props.foreach(_ := false.B)
            look8.io.CIN := ins(0)

            // Generate the LUT structure of the atoms and connect them to the LOOKAHEAD8
            val luts = comp.atoms.zipWithIndex.map {
              case (_: Atom22, i) => buildAtom22(ins(i+1), look8COs(2*i+1) ## look8COs(2*i))
              case (_: Atom14, i) => buildAtom14(ins(i+1), look8COs(2*i+1) ## look8COs(2*i))
              case (atom, _)      =>
                throw new IllegalArgumentException(s"cannot generate hardware for unsupported atom ${atom}")
            }
            luts.zipWithIndex.foreach { case ((_, cos, props), i) =>
              look8CYs(2*i)       := cos(0)
              look8CYs(2*i + 1)   := cos(1)

              look8Props(2*i)     := props(0)
              look8Props(2*i + 1) := props(1)
            }

            // Outputs: [carry out, sum bits]
            io.out := look8COs(2*comp.atoms.length - 1) ## VecInit(luts.reverse.map(_._1)).asUInt

          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = cntr.getClass().getName()
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new VersalCounter(cntr))
      Instance(state.cntrDefs(cntrName))
    }

    /** Function to construct a variable-length counter */
    def construct(cntr: VarLenCounter, len: Int, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class VersalCounter(counter: VarLenCounter, length: Int) extends HardwareCounter((counter.inSigFn(length), counter.outSigFn(length))) {
        /** Different counters require different amounts of logic here */
        counter match {
          case _: RippleSum =>
            val carries = Wire(Vec(length, Bool()))
            val ripples = Wire(Vec(length + 1, Bool()))
            ripples(0) := io.in(0) // transfer in

            // Construct and connect LUTs
            (0 until length).foreach { i =>
              // Boolean functions for the LUT
              val lutFO5 = (ins: Seq[Boolean]) => (ins(0) && ins(1)) || (ins(0) && ins(4)) || (ins(1) && ins(4))
              val lutFO6 = (ins: Seq[Boolean]) => ins(0) ^ ins(1) ^ ins(4)

              // LUT computes S as O6 and C as O5
              // Inputs: (a0 = in(2*i+1), a1 = in(2*i+2), false, false, cascade, false)
              val lut = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
              lut.io.I0 := io.in(2*i + 1)
              lut.io.I1 := io.in(2*i + 2)
              lut.io.I2 := false.B
              lut.io.I3 := false.B
              lut.io.I4 := ripples(i) // cascade
              lut.io.I5 := false.B
              carries(i)   := lut.io.O5
              ripples(i+1) := lut.io.O6
            }

            // Outputs
            io.out := carries.asUInt ## ripples(length)

          case _: DualRailRippleSum =>
            // Construct the top half first
            val topCarries = Wire(Vec(length, Bool()))
            val topRipples = Wire(Vec(length + 1, Bool()))
            topRipples(0) := io.in(0) // transfer 0 in

            (0 until length).foreach { i =>
              // Boolean functions for the LUT
              val lutFO5 = (ins: Seq[Boolean]) => {
                val s = (ins(1) ^ ins(2) ^ ins(3))
                (ins(0) && s) || (ins(0) && ins(4)) || (s && ins(4))
              }
              val lutFO6 = (ins: Seq[Boolean]) => ins.take(5).reduce(_ ^ _)

              // LUT computes S as O6 and C as O5
              // Inputs: (a0 = in(4*i+1), a1 = in(4*i+2), a2 = in(4*i+3), a3 = in(4*i+4), cascade, false)
              val lut = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
              lut.io.I0 := io.in(4*i + 1)
              lut.io.I1 := io.in(4*i + 2)
              lut.io.I2 := io.in(4*i + 3)
              lut.io.I3 := io.in(4*i + 4)
              lut.io.I4 := topRipples(i) // cascade
              lut.io.I5 := false.B
              topCarries(i)   := lut.io.O5
              topRipples(i+1) := lut.io.O6
            }

            // Construct the bottom half second
            val botCarries = Wire(Vec(length, Bool()))
            val botRipples = Wire(Vec(length + 1, Bool()))
            botRipples(0) := io.in(length + 1) // transfer 1 in

            (0 until length).foreach { i =>
              // Boolean functions for the LUT
              val lutFO5 = (ins: Seq[Boolean]) => {
                val c = (ins(1) && ins(2)) || (ins(1) && ins(3)) || (ins(2) && ins(3))
                (ins(0) && c) || (ins(0) && ins(4)) || (c && ins(4))
              }
              val lutFO6 = (ins: Seq[Boolean]) => {
                val c = (ins(1) && ins(2)) || (ins(1) && ins(3)) || (ins(2) && ins(3))
                ins(0) ^ c ^ ins(4)
              }

              // LUT computes S as O6 and C as O5
              // Inputs: (a0 = in(i+n+2), a1 = in(4*i+2), a2 = in(4*i+3), a3 = in(4*i+4), cascade, false)
              val lut = Module(new LUT6_2(genLUT6_2InitString(lutFO5, lutFO6)))
              lut.io.I0 := io.in(i + length + 2)
              lut.io.I1 := io.in(4*i + 2)
              lut.io.I2 := io.in(4*i + 3)
              lut.io.I3 := io.in(4*i + 4)
              lut.io.I4 := botRipples(i) // cascade
              lut.io.I5 := false.B
              botCarries(i)   := lut.io.O5
              botRipples(i+1) := lut.io.O6
            }

            // Outputs
            io.out := botCarries.asUInt ## botRipples(length) ## topCarries.asUInt ## topRipples(length)

          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported variable-length counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = s"${cntr.getClass().getName()}_$len"
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new VersalCounter(cntr, len))
      Instance(state.cntrDefs(cntrName))
    }
  }

  /** Collection of counters for Intel FPGAs
   * 
   * The current library includes no atoms and the following standalone counters:
   * - (2 : 1,1]
   * - (3 : 1,1]
   * - (8 : 1,1,1] (approximate)
   */
  object Intel extends Library {
    /** Counter (2 : 1,1] (half adder) */
    private[Intel] class Counter2_11 extends Counter((Array(2), Array(1, 1)), 1) with Exact

    /** Counter (3 : 1,1] (full adder) */
    private[Intel] class Counter3_11 extends Counter((Array(3), Array(1, 1)), 2) with Exact

    /** Approximate counter (8 : 1,1,1]
     * 
     * Implementation of the counter from Boroumand and Brisk [2019]
     */
    private[Intel] class Counter8_111 extends Counter((Array(8), Array(1, 1, 1)), 4) with Approximate

    /** Collection of exact counters */
    lazy val exactCounters: Seq[Counter] = Seq(
      (new Counter2_11),
      (new Counter3_11)
    )

    /** Collection of approximate and exact counters */
    lazy val approxCounters: Seq[Counter] = exactCounters ++ Seq(
      (new Counter8_111)
    )

    /** Collection of exact variable-length counters */
    lazy val exactVarLenCounters: Seq[VarLenCounter] = Seq()

    /** Collection of approximate and exact variable-length counters */
    lazy val approxVarLenCounters: Seq[VarLenCounter] = exactVarLenCounters ++ Seq()

    /** Function to construct a counter */
    def construct(cntr: Counter, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class IntelCounter(counter: Counter) extends HardwareCounter(counter.sig) {
        /** Depending on the counter, we need to instantiate different amounts
         * of logic here. We wrap existing compressor implementations when possible.
         */
        counter match {
          case _: Counter2_11 =>
            val comp = Module(new Compressor2to2)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            io.out := comp.io.cout ## comp.io.s

          case _: Counter3_11 =>
            val comp = Module(new Compressor3to2)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            comp.io.x3 := io.in(2)
            io.out := comp.io.cout ## comp.io.s

          case _: Counter8_111 =>
            val comp = Module(new Compressor8to3)
            comp.io.x1 := io.in(0)
            comp.io.x2 := io.in(1)
            comp.io.x3 := io.in(2)
            comp.io.x4 := io.in(3)
            comp.io.x5 := io.in(4)
            comp.io.x6 := io.in(5)
            comp.io.x7 := io.in(6)
            comp.io.x8 := io.in(7)
            io.out := comp.io.z2 ## comp.io.z1 ## comp.io.z0

          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = cntr.getClass().getName()
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new IntelCounter(cntr))
      Instance(state.cntrDefs(cntrName))
    }

    /** Function to construct a variable-length counter */
    def construct(cntr: VarLenCounter, len: Int, state: State): Instance[HardwareCounter] = {
      /** Generic extension of the hardware counter for returning */
      class IntelCounter(counter: VarLenCounter, length: Int) extends HardwareCounter((counter.inSigFn(length), counter.outSigFn(length))) {
        /** Different counters require different amounts of logic here */
        counter match {
          case _ => throw new IllegalArgumentException(s"cannot generate hardware for unsupported variable-length counter ${counter}")
        }
      }

      // Store a definition of this hardware counter for future reference
      val cntrName = s"${cntr.getClass().getName()}_$len"
      if (!state.cntrDefs.contains(cntrName))
        state.cntrDefs(cntrName) = Definition(new IntelCounter(cntr, len))
      Instance(state.cntrDefs(cntrName))
    }
  }
}
