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

  /** DMFA dual-mode approximate full-adder IO bundle */
  private[DualModeRCA] class DMFAIO extends FAIO {
    val app = Input(Bool())
  }

  /** DMFA dual-mode approximate full-adder */
  private[DualModeRCA] class DMFA extends Module {
    val io = IO(new DMFAIO)
    io.s := Mux(io.app, io.y, io.x ^ io.y ^ io.cin)
    io.cout := Mux(io.app, io.x, (io.x & io.y) | (io.x & io.cin) | (io.y & io.cin))
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
    fa.io.x    := io.a(i)
    fa.io.y    := io.b(i)
    fa.io.cin  := couts(i)
    fa.io.app  := dec.io.ctrl(i)
    sums(i)    := fa.io.s
    couts(i+1) := fa.io.cout
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
  private[DualModeCLA] abstract class DMCLB extends Module {
    class CLBIO extends FAIO {
      val app = Input(Bool())
      val p   = Output(Bool())
      val g   = Output(Bool())
    }
    val io = IO(new CLBIO)
  }

  /** DMCLB dual-mode carry look-ahead block ver. 1 */
  private[DualModeCLA] class DMCLB1 extends DMCLB {
    val p = io.x ^ io.y
    val g = io.x & io.y
    io.s    := Mux(io.app, io.y, p ^ io.cin)
    io.cout := Mux(io.app, io.x, g | (p & io.cin))
    io.p    := Mux(io.app, io.y, p)
    io.g    := Mux(io.app, io.x, g)
  }

  /** DMCLB dual-mode carry look-ahead block ver. 2 */
  private[DualModeCLA] class DMCLB2 extends DMCLB {
    val p = io.x ^ io.y
    val g = io.x & io.y
    io.s    := Mux(io.app, io.y, p ^ io.cin)
    io.cout := false.B
    io.p    := Mux(io.app, io.y, p)
    io.g    := Mux(io.app, io.x, g)
  }

  /** PGB dual-mode generate and propagate block abstract class */
  private[DualModeCLA] abstract class DMPGB extends Module {
    val io = IO(new Bundle {
      val app  = Input(Bool())
      val pI   = Input(Vec(2, Bool()))
      val gI   = Input(Vec(2, Bool()))
      val cin  = Input(Bool())
      val pO   = Output(Bool())
      val gO   = Output(Bool())
      val cout = Output(Bool())
    })
  }

  /** CMPGB dual-mode generate and propagate block ver. 1 */
  private[DualModeCLA] class DMPGB1 extends DMPGB {
    val p  = io.pI.asUInt.andR
    val g  = io.gI(1) | (io.gI(0) & io.pI(1))
    io.cout := g | (p & io.cin)
    io.pO := Mux(io.app, io.pI(0), p)
    io.gO := Mux(io.app, io.gI(1), g)
  }

  /** CMPGB dual-mode generate and propagate block ver. 2 */
  private[DualModeCLA] class DMPGB2 extends DMPGB {
    val p  = io.pI.asUInt.andR
    val g  = io.gI(1) | (io.gI(0) & io.pI(1))
    io.cout := false.B
    io.pO := Mux(io.app, io.pI(0), p)
    io.gO := Mux(io.app, io.gI(1), g)
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
    clb.io.x   := io.a(i)
    clb.io.y   := io.b(i)
    clb.io.cin := couts(i)
    clb.io.app := dec.io.ctrl(i)
    if ((i & 1) == 0) {
      couts(i+1) := clb.io.cout
    }
    sums(i)     := clb.io.s
    gens(0)(i)  := clb.io.g
    props(0)(i) := clb.io.p
  }

  // ... and next using PGB blocks
  (1 until log2Up(width)+1).foreach { layer => 
    val weight = 1 << layer
    (0 until width / weight).foreach { block => 
      // Odd-index blocks are PGB2s, others are PGB1s
      val pgb = if ((block & 1) == 1) Module(new DMPGB2) else Module(new DMPGB1)
      // Calculate top and bottom indices of (p,g) signals from previous layer
      val (top, bot) = (block * 2, (block * 2) + 1)
      pgb.io.gI(0) := gens(layer-1)(bot)
      pgb.io.gI(1) := gens(layer-1)(top)
      pgb.io.pI(0) := props(layer-1)(bot)
      pgb.io.pI(1) := props(layer-1)(top)
      pgb.io.app := dec.io.ctrl(weight-1)
      // Judging by fig. 8 in the paper, only index-0 blocks have carry-in
      if (block == 0)
        pgb.io.cin := couts(0)
      else
        pgb.io.cin := false.B
      // Only use carry-outs of even-indexed blocks
      if ((block & 1) == 0)
        couts((block + 1) * weight) := pgb.io.cout
      gens(layer)(block)  := pgb.io.gO
      props(layer)(block) := pgb.io.pO
    }
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := couts(width)
}
