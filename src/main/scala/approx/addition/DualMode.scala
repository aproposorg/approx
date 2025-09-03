package approx.addition

import chisel3._
import chisel3.util.log2Up

/** Dual-mode approximate adder IO bundle
 * 
 * @param width the width of the adder
 * 
 * App input is used for enabling approximations.
 */
class DualModeIO(width: Int) extends AdderIO(width) {
  val app = Input(UInt(log2Up(width).W))
}

/** Dual-mode approximate mode to control signal decoder 
 * 
 * @param width the width of the targeted adder
 * 
 * This parameterized version is probably sub-optimal.
 */
class DualModeDecoder(width: Int) extends Module {
  val io = IO(new Bundle {
    val app  = Input(UInt(log2Up(width).W))
    val ctrl = Output(UInt(width.W))
  })

  val ctrl = Wire(Vec(width, Bool()))
  (0 until width).foreach { i =>
    ctrl(i) := (io.app -& i.U) > 0.U
  }
  io.ctrl := ctrl.asUInt
}

/** Dual-mode ripple-carry adder
 * 
 * @param width the width of the adder
 * 
 * Implementation of the adder from Raha et al. [2016]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class DualModeRCA(width: Int) extends Module {
  val io = IO(new DualModeIO(width))

  /** DMFA dual-mode approximate full-adder */
  private[DualModeRCA] class DMFA extends RawModule {
    class DMFAIO extends FAIO {
      val app = Input(Bool())
    }
    val dmfaio = IO(new DMFAIO)
    dmfaio.s := Mux(dmfaio.app, dmfaio.y, dmfaio.x ^ dmfaio.y ^ dmfaio.cin)
    dmfaio.cout := Mux(dmfaio.app, dmfaio.x, (dmfaio.x & dmfaio.y) | (dmfaio.x & dmfaio.cin) | (dmfaio.y & dmfaio.cin))
  }

  // Instantiate a decoder
  val dec = Module(new DualModeDecoder(width))
  dec.io.app := io.app

  // Generate the sum and carry bits
  val sums  = Wire(Vec(width, Bool()))
  val couts = Wire(Vec(width+1, Bool()))
  couts(0) := io.cin
  (0 until width).foreach { i =>
    val fa = Module(new DMFA)
    fa.dmfaio.x    := io.a(i)
    fa.dmfaio.y    := io.b(i)
    fa.dmfaio.cin  := couts(i)
    fa.dmfaio.app  := dec.io.ctrl(i)
    sums(i)    := fa.dmfaio.s
    couts(i+1) := fa.dmfaio.cout
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := couts(width)
}

/** Dual-mode carry look-ahead adder
 * 
 * @param width the width of the adder
 * 
 * Implementation of the adder from Raha et al. [2016]
 * 
 * Does not inherit from [[Adder]] because of IO mismatch.
 */
class DualModeCLA(width: Int) extends Module {
  val io = IO(new DualModeIO(width))

  /** CLB dual-mode carry look-ahead block abstract class */
  private[DualModeCLA] abstract class DMCLB extends RawModule {
    class CLBIO extends FAIO {
      val app = Input(Bool())
      val p   = Output(Bool())
      val g   = Output(Bool())
    }
    val clbio = IO(new CLBIO)
  }

  /** DMCLB dual-mode carry look-ahead block ver. 1 */
  private[DualModeCLA] class DMCLB1 extends DMCLB {
    val p = clbio.x ^ clbio.y
    val g = clbio.x & clbio.y
    clbio.s    := Mux(clbio.app, clbio.y, p ^ clbio.cin)
    clbio.cout := Mux(clbio.app, clbio.x, g | (p & clbio.cin))
    clbio.p    := Mux(clbio.app, clbio.y, p)
    clbio.g    := Mux(clbio.app, clbio.x, g)
  }

  /** DMCLB dual-mode carry look-ahead block ver. 2 */
  private[DualModeCLA] class DMCLB2 extends DMCLB {
    val p = clbio.x ^ clbio.y
    val g = clbio.x & clbio.y
    clbio.s    := Mux(clbio.app, clbio.y, p ^ clbio.cin)
    clbio.cout := false.B
    clbio.p    := Mux(clbio.app, clbio.y, p)
    clbio.g    := Mux(clbio.app, clbio.x, g)
  }

  /** PGB dual-mode generate and propagate block abstract class */
  private[DualModeCLA] abstract class DMPGB extends Module {
    class PGBIO extends Bundle {
      val app  = Input(Bool())
      val pI   = Input(Vec(2, Bool()))
      val gI   = Input(Vec(2, Bool()))
      val cin  = Input(Bool())
      val pO   = Output(Bool())
      val gO   = Output(Bool())
      val cout = Output(Bool())
    }
    val pgbio = IO(new PGBIO)
  }

  /** CMPGB dual-mode generate and propagate block ver. 1 */
  private[DualModeCLA] class DMPGB1 extends DMPGB {
    val p  = pgbio.pI.asUInt.andR
    val g  = pgbio.gI(1) | (pgbio.gI(0) & pgbio.pI(1))
    pgbio.cout := g | (p & pgbio.cin)
    pgbio.pO := Mux(pgbio.app, pgbio.pI(0), p)
    pgbio.gO := Mux(pgbio.app, pgbio.gI(1), g)
  }

  /** CMPGB dual-mode generate and propagate block ver. 2 */
  private[DualModeCLA] class DMPGB2 extends DMPGB {
    val p  = pgbio.pI.asUInt.andR
    val g  = pgbio.gI(1) | (pgbio.gI(0) & pgbio.pI(1))
    pgbio.cout := false.B
    pgbio.pO := Mux(pgbio.app, pgbio.pI(0), p)
    pgbio.gO := Mux(pgbio.app, pgbio.gI(1), g)
  }

  // Instantiate a decoder
  val dec = Module(new DualModeDecoder(width))
  dec.io.app := io.app

  // Generate the sum and carry bits
  val sums  = Wire(Vec(width, Bool()))
  val couts = Wire(Vec(width+1, Bool()))
  val gens  = WireDefault(VecInit(IndexedSeq.fill(log2Up(width)+1) { VecInit(IndexedSeq.fill(width) { false.B }) }))
  val props = WireDefault(VecInit(IndexedSeq.fill(log2Up(width)+1) { VecInit(IndexedSeq.fill(width) { false.B }) }))
  couts(0) := io.cin

  // ... first using CLB blocks
  (0 until width).foreach { i => 
    val clb = if ((i & 1) == 1) Module(new DMCLB2) else Module(new DMCLB1)
    clb.clbio.x   := io.a(i)
    clb.clbio.y   := io.b(i)
    clb.clbio.cin := couts(i)
    clb.clbio.app := dec.io.ctrl(i)
    if ((i & 1) == 0) {
      couts(i+1) := clb.clbio.cout
    }
    sums(i)     := clb.clbio.s
    gens(0)(i)  := clb.clbio.g
    props(0)(i) := clb.clbio.p
  }

  // ... and next using PGB blocks
  (1 until log2Up(width)+1).foreach { layer => 
    val weight = 1 << layer
    (0 until width / weight).foreach { block => 
      // Odd-index blocks are PGB2s, others are PGB1s
      val pgb = if ((block & 1) == 1) Module(new DMPGB2) else Module(new DMPGB1)
      // Calculate top and bottom indices of (p,g) signals from previous layer
      val (top, bot) = (block * 2, (block * 2) + 1)
      pgb.pgbio.gI(0) := gens(layer-1)(bot)
      pgb.pgbio.gI(1) := gens(layer-1)(top)
      pgb.pgbio.pI(0) := props(layer-1)(bot)
      pgb.pgbio.pI(1) := props(layer-1)(top)
      pgb.pgbio.app := dec.io.ctrl(weight-1)
      // Judging by fig. 8 in the paper, only index-0 blocks have carry-in
      if (block == 0)
        pgb.pgbio.cin := couts(0)
      else
        pgb.pgbio.cin := false.B
      // Only use carry-outs of even-indexed blocks
      if ((block & 1) == 0)
        couts((block + 1) * weight) := pgb.pgbio.cout
      gens(layer)(block)  := pgb.pgbio.gO
      props(layer)(block) := pgb.pgbio.pO
    }
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := couts(width)
}
