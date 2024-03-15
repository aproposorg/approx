package approx.addition

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util.{log2Up, HasBlackBoxResource}

/** Accurate half-adder */
class HalfAdder extends HA {
  io.s := io.x ^ io.y
  io.cout := io.x & io.y
}

/** Accurate full-adder */
class FullAdder extends FA {
  io.s := io.x ^ io.y ^ io.cin
  io.cout := (io.x & io.y) | (io.x & io.cin) | (io.y & io.cin)
}

/** Exact ripple carry adder
 * 
 * @param width the width of the adder
 */
class RCA(width: Int) extends Adder(width) {
  val sums = Wire(Vec(width, Bool()))
  val carries = Wire(Vec(width+1, Bool()))
  carries(0) := io.cin
  (0 until width).foreach { i =>
    val fa = Module(new FullAdder)
    fa.io.x   := io.a(i)
    fa.io.y   := io.b(i)
    fa.io.cin := carries(i)
    sums(i)      := fa.io.s
    carries(i+1) := fa.io.cout
  }

  // Combine results and output
  io.s := sums.asUInt
  io.cout := carries(width)
}

/** Exact ripple carry adder blacbox
 * 
 * @param width the width of the adder
 */
private[addition] class VRCA(val width: Int) extends BlackBox(Map("width" -> IntParam(width))) with HasBlackBoxResource {
  val io = IO(new AdderIO(width))
  addResource("/VRCA.v")
}

/** Exact ripple carry adder from blackbox
 * 
 * @param width the width of the adder
 */
class BlackboxRCA(width: Int) extends Adder(width) {
  val rca = Module(new VRCA(width))
  io <> rca.io
}

/** Carry look-ahead-style carry generation
 * 
 * @param width the width of the generator
 */
private[addition] class CarryGen(width: Int) extends Module {
  val io = IO(new Bundle {
    val p = Input(UInt(width.W))
    val g = Input(UInt(width.W))
    val cin = Input(Bool())
    val carries = Output(UInt(width.W))
    val cout = Output(Bool())
  })

  val cs = Wire(Vec(width+1, Bool()))
  cs(0) := io.cin
  (0 until width).foreach { i => cs(i+1) := io.g(i) | (io.p(i) & cs(i)) }
  io.carries := cs.asUInt()(width-1, 0)
  io.cout    := cs(width)
}

/** Exact carry-lookahead adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than the width)
 */
class CLA(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

  // Split operands
  val aVec = io.a.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val bVec = io.b.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val pVec = (io.a ^ io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val gVec = (io.a & io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))

  // Carry generation
  val carries = Wire(Vec(stages, UInt(stageWidth.W)))
  val couts = Wire(Vec(stages+1, Bool()))
  couts(0) := io.cin
  (0 until stages).foreach { i =>
    val cg = Module(new CarryGen(stageWidth))
    cg.io.p   := pVec(i)
    cg.io.g   := gVec(i)
    cg.io.cin := couts(i)
    carries(i) := cg.io.carries
    couts(i+1) := cg.io.cout
  }

  // Combine results and output
  io.s := pVec.asUInt ^ carries.asUInt
  io.cout := couts(stages)
}

/** Exact two-layer carry-lookahead adder
 * 
 * @param w the width of the adder
 * @param stages the number of adder stages in each layer (must be less than the width)
 */
class CLA2(width: Int, val stages: Tuple2[Int, Int]) extends Adder(width) {
  val (stageWidth1, stageWidth2) = (width / stages._1, stages._1 / stages._2)
  require(stageWidth1 >= 2, "width of stages in first layer must be at least 2")
  require(stageWidth2 >= 1, "width of stages in second layer must be at least 1")
  require(stages._2 <= stages._1, "number of stages in second layer must be less than or equal to the number of stages in first layer")
  require(stages._1 < width, "number of stages must be less than the width")

  // Split operands
  val aVec = io.a.asTypeOf(Vec(stages._1, UInt(stageWidth1.W)))
  val bVec = io.b.asTypeOf(Vec(stages._1, UInt(stageWidth1.W)))
  val pVec = (io.a ^ io.b).asTypeOf(Vec(stages._1, UInt(stageWidth1.W)))
  val gVec = (io.a & io.b).asTypeOf(Vec(stages._1, UInt(stageWidth1.W)))

  // Carry generation signals and modules
  val carries = Wire(Vec(stages._1, UInt(stageWidth1.W)))
  val couts   = Wire(Vec(stages._1+1, Bool()))
  val clas    = Array.fill(stages._1) { Module(new CarryGen(stageWidth1)) }
  val clgs    = Array.fill(stages._2) { Module(new CarryGen(stageWidth2)) }

  // Connect layer 1 CLAs
  (0 until stages._1).foreach { i =>
    clas(i).io.p   := pVec(i)
    clas(i).io.g   := gVec(i)
    clas(i).io.cin := couts(i)
    carries(i) := clas(i).io.carries
  }

  // Group alive and generate generation
  val alives  = (pVec.asUInt | gVec.asUInt).asTypeOf(Vec(stages._1, UInt(stageWidth1.W)))
  val gAlives = VecInit(alives.map { _.andR }).asUInt
  val gGens   = VecInit((0 until stages._1).map { i =>
    (gVec(i)(stageWidth1-1) ## VecInit((0 until stageWidth1-1).map { j =>
      alives(i)(stageWidth1-1, j+1).andR & gVec(i)(j)
    }).asUInt).orR
  }).asUInt

  // Connect layer 2 CLGs
  couts(0) := io.cin
  (0 until stages._2).foreach { i =>
    clgs(i).io.p   := gAlives((i + 1) * stageWidth2 - 1, i * stageWidth2)
    clgs(i).io.g   := gGens((i + 1) * stageWidth2 - 1, i * stageWidth2)
    clgs(i).io.cin := couts(i * stageWidth2)

    // Connect carry outs
    (i * stageWidth2 + 1 until (i + 1) * stageWidth2).foreach { j =>
      couts(j) := clgs(i).io.carries(j - (i * stageWidth2))
    }
    couts((i + 1) * stageWidth2) := clgs(i).io.cout
  }

  // Combine results and output
  io.s := pVec.asUInt ^ carries.asUInt
  io.cout := couts(stages._1)
}

/** Exact carry-select adder
 * 
 * @param width the width of the adder
 * @param stages the number of adder stages (must be less than the width)
 */
class CSA(width: Int, val stages: Int) extends Adder(width) {
  val stageWidth = width / stages
  require(stageWidth >= 2, "width of stages must be at least 2")
  require(stages < width, "number of stages must be less than the width")

  // Split operands
  val aVec = io.a.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val bVec = io.b.asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val pVec = (io.a ^ io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))
  val gVec = (io.a & io.b).asTypeOf(Vec(stages, UInt(stageWidth.W)))

  // Carry generation
  val carries = Wire(Vec(stages, UInt(stageWidth.W)))
  val couts = Wire(Vec(stages+1, Bool()))
  couts(0) := io.cin
  (0 until stages).foreach { i =>
    // For false input
    val cgF = Module(new CarryGen(stageWidth))
    cgF.io.p   := pVec(i)
    cgF.io.g   := gVec(i)
    cgF.io.cin := false.B

    // For true input
    val cgT = Module(new CarryGen(stageWidth))
    cgT.io.p   := pVec(i)
    cgT.io.g   := gVec(i)
    cgT.io.cin := true.B

    // Select the right outputs
    carries(i) := Mux(couts(i), cgT.io.carries, cgF.io.carries)
    couts(i+1) := Mux(couts(i), cgT.io.cout, cgF.io.cout)
  }

  // Combine results and output
  io.s := pVec.asUInt ^ carries.asUInt
  io.cout := couts(stages)
}

/** Exact parallel prefix adder base class
 * 
 * @param width the width of the adder
 * 
 * The 'fanout' architecture models the PPA from Ercegovac and Lang, Fig. 2.19, 
 * while the 'delay' architecture models the PPA from Fig. 2.20. The 'fanout' 
 * architecture is likely to have higher latency but lower area than the 
 * 'delay' architecture.
 * 
 * @todo Remove redundant extension bits in this design.
 */
abstract class PPA(width: Int) extends Adder(width) {
  /** Bundle of generate and alive bits */
  private[addition] class GenAlivePair extends Bundle {
    val g = Bool()
    val a = Bool()
  }

  /** Compute a full-dot operator
   * 
   * @param l the left `GenAlivePair` bundle
   * @param r the right boolean
   * @return a boolean prefix bit
   */
  private[addition] def fullDot(l: GenAlivePair, r: Bool): Bool = l.g | (l.a & r)

  /** Compute an empty-dot operator
   * 
   * @param l a `GenAlivePair` bundle
   * @param r a `GenAlivePair` bundle
   * @return a combined `GenAlivePair` bundle from `l` and `r`
   */
  private[addition] def emptyDot(l: GenAlivePair, r: GenAlivePair): GenAlivePair = {
    val res = Wire(new GenAlivePair)
    res.g := l.g | (l.a & r.g)
    res.a := l.a & r.a
    res
  }

  /** Recursively build the parallel prefix tree
   * 
   * @param ins the inputs to the current level of the tree
   * @param lvl the index of the current level of the tree
   */
  private[addition] def tree(ins: Seq[Any], lvl: Int = 0): UInt

  // The architecture generation part of the design only works for power-of-2 
  // bit-width, so extend the operands thereafter
  def sext(op: UInt, twidth: Int): UInt = {
    if (twidth <= op.getWidth) op(twidth-1, 0)
    else VecInit(Seq.fill(twidth - op.getWidth)(op(op.getWidth-1))).asUInt ## op
  }
  val inWidth = 1 << log2Up(width)
  val inA = sext(io.a, inWidth)
  val inB = sext(io.b, inWidth)

  // Compute the generate, alive, and propagate signals from the extended inputs
  val g = inA & inB
  val a = inA | inB
  val p = inA ^ inB

  // Compute the inputs to the parallel prefix tree
  val inLevel = (io.cin +: (0 until inWidth).map { i =>
    val res = Wire(new GenAlivePair)
    res.g := g(i)
    res.a := a(i)
    res
  })

  // Inheriting architectures must define the output from the parallel prefix 
  // tree for it to be used in sum and carry-out generation
  val outLevel = tree(inLevel)
  io.s    := p ^ outLevel
  io.cout := outLevel(width)
}

/** Exact parallel prefix adder with maximum fanout of three
 * 
 * @param width the width of the adder
 * 
 * Models the PPA from Ercegovac and Lang, Fig. 2.19.
 */
class LowFanoutPPA(width: Int) extends PPA(width) {
  private[addition] def tree(ins: Seq[Any], lvl: Int = 0): UInt = {
    require(ins.forall(in => in.isInstanceOf[GenAlivePair] || in.isInstanceOf[Bool]))

    // Compute the inputs to the next level
    val next = if (lvl == 0) {
      // Treat the first level differently from the remaining ones
      (0 until ins.size).map { i =>
        if ((i & 0x1) == 1) (ins(i), ins(i-1)) match {
          case (l: GenAlivePair, r: GenAlivePair) => emptyDot(l, r)
          case (l: GenAlivePair, r: Bool) => fullDot(l, r)
          case _ => // should never occur
        } else ins(i)
      }
    } else {
      // Two bit positions share the same right element
      val lower = lvl << 1
      val upper = scala.math.min(lower + 2, ins.size)
      val r = ins(lower - 1)
      ins.take(lower) ++ (lower until upper).map { i =>
        (ins(i), r) match {
          case (l: GenAlivePair, r: Bool) => fullDot(l, r)
          case _ => // should never occur
        }
      } ++ ins.drop(upper)
    }

    // Finalize this level or the whole tree
    if (next.exists(_.isInstanceOf[GenAlivePair])) tree(next, lvl + 1)
    else VecInit(next.asInstanceOf[Seq[Bool]]).asUInt
  }
}

/** Exact parallel prefix adder with minimum number of levels
 * 
 * @param width the width of the adder
 * 
 * Models the PPA from Ercegovac and Lang, Fig. 2.20.
 */
class MinLevelsPPA(width: Int) extends PPA(width) {
  private[addition] def tree(ins: Seq[Any], lvl: Int = 0): UInt = {
    require(ins.forall(in => in.isInstanceOf[GenAlivePair] || in.isInstanceOf[Bool]))

    // Compute the inputs to the next level or the final result
    val skip = 1 << lvl
    val next = ins.take(skip) ++ (skip until ins.size).map { i =>
      (ins(i), ins(i - skip)) match {
        case (l: GenAlivePair, r: GenAlivePair) => emptyDot(l, r)
        case (l: GenAlivePair, r: Bool) => fullDot(l, r)
        case _ => // should never occur
      }
    }

    // Finalize this level or the whole tree
    if (next.exists(_.isInstanceOf[GenAlivePair])) tree(next, lvl + 1)
    else VecInit(next.asInstanceOf[Seq[Bool]]).asUInt
  }
}
