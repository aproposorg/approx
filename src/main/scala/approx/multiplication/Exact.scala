package approx.multiplication

import approx.addition.{HalfAdder, FullAdder}
import ReductionType._

import chisel3._
import chisel3.util._

/** 3:2 compressor */
class Compressor3to2 extends C3to2 {
  val add = Module(new FullAdder)
  add.io.x   := io.x1
  add.io.y   := io.x2
  add.io.cin := io.x3
  io.s    := add.io.s
  io.cout := add.io.cout
}

/** 4:2 compressor */
class Compressor4to2 extends C4to2 {
  val add1 = Module(new FullAdder)
  val add2 = Module(new FullAdder)
  add1.io.x   := io.x1
  add1.io.y   := io.x2
  add1.io.cin := io.x3
  add2.io.x   := add1.io.s
  add2.io.y   := io.x4
  add2.io.cin := io.cin
  io.s    := add2.io.s
  io.c    := add2.io.cout
  io.cout := add1.io.cout
}

/** Optimized 4:2 compressor
 * 
 * Interpretation of the compressor of Chang et al. [2004]
 */
class Compressor4to2Opt extends C4to2 {
  val sint = (io.x1 ^ io.x2) ^ (io.x3 ^ io.x4)
  val cint = !((io.x1 & io.x2) | (io.x3 & io.x4))
  io.s := sint ^ io.cin
  io.c := (sint & io.cin) | (!sint | !cint)
  io.cout := (io.x1 | io.x2) & (io.x3 | io.x4)
}

/** 5:3 compressor */
class Compressor5to3 extends C5to3 {
  val add1 = Module(new FullAdder)
  val add2 = Module(new FullAdder)
  val add3 = Module(new HalfAdder)
  add1.io.x   := io.x1
  add1.io.y   := io.x2
  add1.io.cin := io.x3
  add2.io.x   := add1.io.s
  add2.io.y   := io.x4
  add2.io.cin := io.x5
  add3.io.x   := add1.io.cout
  add3.io.y   := add2.io.cout
  io.s  := add2.io.s
  io.c1 := add3.io.s
  io.c2 := add3.io.cout
}

/** 7:3 compressor */
class Compressor7to3 extends C7to3 {
  val add1 = Module(new FullAdder)
  val add2 = Module(new FullAdder)
  val add3 = Module(new FullAdder)
  val add4 = Module(new FullAdder)
  add1.io.x   := io.x1
  add1.io.y   := io.x2
  add1.io.cin := io.x3
  add2.io.x   := io.x4
  add2.io.y   := io.x5
  add2.io.cin := io.x6
  add3.io.x   := io.x7
  add3.io.y   := add1.io.s
  add3.io.cin := add2.io.s
  add4.io.x   := add1.io.cout
  add4.io.y   := add2.io.cout
  add4.io.cin := add3.io.cout
  io.s  := add3.io.s
  io.c1 := add4.io.s
  io.c2 := add4.io.cout
}

/** Exact 2x2-bit multiplier */
class TwoXTwo extends TwoXTwoMult {
  val ha1 = Module(new HalfAdder)
  val ha2 = Module(new HalfAdder)
  ha1.io.x := io.a(1) & io.b(0)
  ha1.io.y := io.a(0) & io.b(1)
  ha2.io.x := ha1.io.cout
  ha2.io.y := io.a(1) & io.b(1)
  io.p := ha2.io.cout ## ha2.io.s ## ha1.io.s ## (io.a(0) & io.b(0))
}

/** Radix 2 combinational multiplier
 * 
 * @param width the width of the multiplier
 * @param red the reduction type to use (one of (default) [[NaiveReduction]], [[ColumnReduction3to2]] and [[ColumnReduction5to3]])
 * 
 * Only works for two's complement numbers, cf. Ercegovac and Lang [2004]
 * 
 * @todo Add reduction using 4:2 compressors.
 */
class Radix2Multiplier(width: Int, val red: ReductionType) extends Multiplier(width) {
  /** Partial product reduction with 3:2 compressors
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods three (potentially, partially reduced) partial products
   * @return the reduced partial products
   * 
   * Both bpos and pprods must be 3 rows of 2*w bits
   */
  private[Radix2Multiplier] def columnReduce3to2(bpos: Array[Array[Boolean]], pprods: Vec[Vec[Bool]]) = {
    require(bpos.length == 3 && bpos(0).length == 2*width)
    require(pprods.length == 3 && pprods(0).length == 2*width)
    var bposRes   = (0 until 3).map { _ => Array.fill(2*width) { false } }.toArray
    val pprodsRes = WireDefault(VecInit(Seq.fill(3) {
      VecInit(Seq.fill(2*width) { false.B })
    }))
    (0 until 2*width).foreach { i =>
      val count = (0 until 3).map { j => if (bpos(j)(i)) 1 else 0 }.reduce(_ + _)
      count match {
        case 1 => // Pass bit through
          bposRes(0)(i) = true
          pprodsRes(0)(i) := pprods(0)(i)
        case 2 => // Pass bits through
          bposRes(0)(i) = true
          bposRes(2)(i) = true
          pprodsRes(0)(i) := pprods(0)(i)
          pprodsRes(2)(i) := pprods(1)(i)
        case 3 => // Insert 3:2 compressor
          bposRes(0)(i) = true
          val comp    = Module(new Compressor3to2)
          comp.io.x1 := pprods(0)(i)
          comp.io.x2 := pprods(1)(i)
          comp.io.x3 := pprods(2)(i)
          pprodsRes(0)(i) := comp.io.s
          if (i+1 < 2*width) {
            bposRes(1)(i+1)    = true
            pprodsRes(1)(i+1) := comp.io.cout
          }
        case _ =>
      }
    }
    (bposRes, pprodsRes)
  }

  /** Partial product reduction with 5:3 compressors
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods three (potentially, partially reduced) partial products
   * @return the reduced partial products
   * 
   * Both bpos and pprods must be 3 rows of 2*w bits
   */
  private[Radix2Multiplier] def columnReduce5to3(bpos: Array[Array[Boolean]], pprods: Vec[Vec[Bool]]) = {
    require(bpos.length == 5 && bpos(0).length == 2*width)
    require(pprods.length == 5 && pprods(0).length == 2*width)
    var bposRes = (0 until 5).map { _ => Array.fill(2*width) { false } }.toArray
    val pprodsRes = WireDefault(VecInit(Seq.fill(5) {
      VecInit(Seq.fill(2*width) { false.B })
    }))
    (0 until 2*width).foreach { i =>
      val count = (0 until 5).map { j => if (bpos(j)(i)) 1 else 0 }.reduce(_ + _)
      count match {
        case 1 => // Pass bit through
          bposRes(0)(i) = true
          pprodsRes(0)(i) := pprods(0)(i)
        case 2 => // Pass bits through
          bposRes(0)(i) = true
          bposRes(3)(i) = true
          pprodsRes(0)(i) := pprods(0)(i)
          pprodsRes(3)(i) := pprods(1)(i)
        case 3 => // Pass bits through
          bposRes(0)(i) = true
          bposRes(3)(i) = true
          bposRes(4)(i) = true
          pprodsRes(0)(i) := pprods(0)(i)
          pprodsRes(3)(i) := pprods(1)(i)
          pprodsRes(4)(i) := pprods(2)(i)
        case 4 => // Insert 5:3 compressor (one low input)
          bposRes(0)(i) = true
          val comp = Module(new Compressor5to3)
          comp.io.x1 := pprods(0)(i)
          comp.io.x2 := pprods(1)(i)
          comp.io.x3 := pprods(2)(i)
          comp.io.x4 := pprods(3)(i)
          comp.io.x5 := false.B
          pprodsRes(0)(i) := comp.io.s
          if (i+1 < 2*width) {
            bposRes(1)(i+1)    = true
            pprodsRes(1)(i+1) := comp.io.c1
          }
          if (i+2 < 2*width) {
            bposRes(2)(i+2)    = true
            pprodsRes(2)(i+2) := comp.io.c2
          }
        case 5 => // Insert 5:3 compressor
          bposRes(0)(i) = true
          val comp = Module(new Compressor5to3)
          comp.io.x1 := pprods(0)(i)
          comp.io.x2 := pprods(1)(i)
          comp.io.x3 := pprods(2)(i)
          comp.io.x4 := pprods(3)(i)
          comp.io.x5 := pprods(4)(i)
          pprodsRes(0)(i)   := comp.io.s
          if (i+1 < 2*width) {
            bposRes(1)(i+1) = true
            pprodsRes(1)(i+1) := comp.io.c1
          }
          if (i+2 < 2*width) {
            bposRes(2)(i+2) = true
            pprodsRes(2)(i+2) := comp.io.c2
          }
        case _ =>
      }
    }
    (bposRes, pprodsRes)
  }

  /** Move bits up in the partial product matrix
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods 2D matrix of partial product bits
   * @return a new 2D matrix of partial product bits with as many partial product bits moved toward row 0 as possible
   */
  private[Radix2Multiplier] def moveUp(bpos: Array[Array[Boolean]], pprods: Vec[Vec[Bool]]) = {
    var bposRes   = bpos.clone
    val pprodsRes = WireDefault(VecInit(Seq.fill(pprods.length) {
      VecInit(Seq.fill(2*width) { false.B })
    }))
    var count = 0
    (0 until 2*width).foreach { c =>
      count = 0
      (0 until bposRes.length).foreach { r =>
        if (bposRes(r)(c)) {
          // Pass bits over
          pprodsRes(count)(c) := pprods(r)(c)

          // Update array
          bposRes(r)(c) = false
          bposRes(count)(c) = true
          count += 1
        }
      }
    }
    (bposRes, pprodsRes)
  }

  /** Iterative partial product reduction
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods 2D matrix of partial product bits
   * @return the reduced final product
   */
  private[Radix2Multiplier] def columnReduce(bpos: Array[Array[Boolean]], pprods: Vec[Vec[Bool]]) = {
    var bposRes   = bpos.clone
    var pprodsRes = WireDefault(pprods)
    var iter = 1
    while (bposRes.length > 2) {
      iter += 1
      var bposL   = List[Array[Array[Boolean]]]()
      var pprodsL = List[Vec[Vec[Bool]]]()

      red match {
        case ColumnReduction3to2 =>
          // Column reduce in chunks of three rows
          val chunks = bposRes.length / 3
          (0 until chunks).foreach { i =>
            val res = columnReduce3to2(
              bposRes.slice(i*3, (i+1)*3),
              VecInit(pprodsRes.slice(i*3, (i+1)*3))
            )
            bposL   :+= res._1
            pprodsL :+= res._2
          }

          // Copy over non-reduced rows
          if (chunks * 3 != bposRes.length) {
            bposL   :+= bposRes.slice(chunks * 3, bposRes.length)
            pprodsL :+= VecInit(pprodsRes.slice(chunks * 3, bposRes.length))
          }

        case ColumnReduction5to3 =>
          // Column reduce in chunks of five rows (if possible)
          val chunks = bposRes.length / 5
          (0 until chunks).foreach { i =>
            val res = columnReduce5to3(
              bposRes.slice(i*5, (i+1)*5),
              VecInit(pprodsRes.slice(i*5, (i+1)*5))
            )
            bposL   :+= res._1
            pprodsL :+= res._2
          }

          // Column reduce chunk of three rows if left over
          val exChunk = if (bposRes.length - (chunks * 5) >= 3) 1 else 0
          if (exChunk != 0) {
            val (bot, top) = (chunks * 5, chunks * 5 + 3)
            val res = columnReduce3to2(
              bposRes.slice(bot, top),
              VecInit(pprodsRes.slice(bot, top))
            )
            bposL   :+= res._1
            pprodsL :+= res._2
          }

          // Copy over non-reduced rows
          if ((chunks * 5) + (exChunk * 3) != bposRes.length) {
            val bot = (chunks * 5) + (exChunk * 3)
            bposL   :+= bposRes.slice(bot, bposRes.length)
            pprodsL :+= VecInit(pprodsRes.slice(bot, bposRes.length))
          }
      }

      // Bring reduced partial products into a common array
      bposRes = bposL.reduce(_ ++ _).toArray
      val pprodsLRed  = Wire(Vec(bposRes.length, Vec(2*width, Bool())))
      var r = 0
      (0 until pprodsL.length).foreach { i =>
        for (l <- pprodsL(i)) {
          pprodsLRed(r) := l
          r += 1
        }
      }

      // Move bits up in the array
      val (movBpos, movPprods) = moveUp(bposRes, pprodsLRed)

      // Reduce size of array
      r = movBpos.length - 1
      while (movBpos(r).forall(!_)) {
        r -= 1
      }
      bposRes   = movBpos.slice(0, r+1)
      pprodsRes = WireDefault(VecInit(movPprods.slice(0, r+1)))
    }

    // Add results (pprodsRes is two rows)
    val sums  = Wire(Vec(2*width, Bool()))
    val couts = Wire(Vec(2*width, Bool()))
    sums(0)  := pprodsRes(0)(0) // always only one bit in use in 
    couts(0) := false.B         // the first column 
    (1 until 2*width).foreach { i =>
      if (bposRes(0)(i) && bposRes(1)(i)) {
        // Insert full adder
        val fa     = Module(new FullAdder)
        fa.io.x   := pprodsRes(0)(i)
        fa.io.y   := pprodsRes(1)(i)
        fa.io.cin := couts(i-1)
        sums(i)   := fa.io.s
        couts(i)  := fa.io.cout
      } else {
        // Insert half adder
        val ha    = Module(new HalfAdder)
        ha.io.x  := pprodsRes(0)(i)
        ha.io.y  := couts(i-1)
        sums(i)  := ha.io.s
        couts(i) := ha.io.cout
      }
    }
    sums.asUInt
  }

  // Generate partial products
  val pprods = Wire(Vec(width, Vec(2*width, Bool())))
  (0 until width).foreach { r =>
    val aVec   = WireDefault(io.a(width-2, 0))
    val common = VecInit(aVec.asTypeOf(Vec(width-1, Bool())).map(_ && io.b(r))).asUInt
    if (r == 0) {
      pprods(r) := (io.b(width-1) ## !(io.a(width-1) && io.b(r)) ## common).asTypeOf(pprods(r))
    } else if (r == width-1) {
      aVec      := ~io.a(width-2, 0)
      pprods(r) := (true.B ## !(!io.a(width-1) && io.b(r)) ## common(common.getWidth-1, 1) ## !(io.a(0) && io.b(r)) ## 0.U(r.W)).asTypeOf(pprods(r))
    } else {
      pprods(r) := (!(io.a(width-1) && io.b(r)) ## common ## 0.U(r.W)).asTypeOf(pprods(r))
    }
  }

  // Generate partial product reduction
  red match {
    case NaiveReduction =>
      io.p := VecInit(pprods.map(_.asUInt)).reduceTree(_ + _)
    case _ =>
      val (movBpos, movPprods) = moveUp(radix2PProdGen(width), pprods)
      io.p := columnReduce(movBpos, movPprods)
  }
}

/** Exact unsigned recursive multiplier
 * 
 * @param width the width of the multiplier
 * 
 * Implementation of Karatsuba's algorithm cf. Danysh and Swartzlander [1998]
 */
private[approx] class RecurMult(width: Int) extends Multiplier(width) {
  /** Calculate absolute difference and sign of two operands
   * 
   * @param x0 the first operand
   * @param x1 the second operand
   * @return a pair of (sign, absolute difference)
   */
  private[RecurMult] def diff(x0: UInt, x1: UInt) = {
    val d = x0 -& x1
    val (sign, num) = (d(d.getWidth-1), d(d.getWidth-2, 0))
    (sign, Mux(sign, -num, num))
  }

  // Generate multiplication hardware
  if (width <= 2) {
    // If this is a 2-bit multiplication, simply instantiate a TwoXTwo module
    val mult = Module(new TwoXTwo)
    mult.io.a := io.a
    mult.io.b := io.b
    io.p := mult.io.p
  } else {
    // Otherwise, follow the steps of the algorithm. I.e.,
    // 1) split the operands (evenly) in two $a1a0$ and $b1b0$,
    // 2) calculate three sub-products $z2 = a1*b1$, 
    //    $z1 = (a0-a1)*(b1-b0)+z2+z0$, $z0 = a0*b0$, and
    // 3) combine results $p = (z2 << (2*m)) + (z1 << m) + z0$.
    val m = if ((width & 1) == 1) 1 << (log2Up(width) - 1) else width / 2
    val (a1, a0) = (io.a(width-1, m), io.a(m-1, 0))
    val (b1, b0) = (io.b(width-1, m), io.b(m-1, 0))
    val mults    = Array.fill(3) { Module(new RecurMult(m)) }

    // First, the two easy sub-products z2 and z0
    val z2 = WireDefault(0.U((2*width).W))
    mults(2).io.a := a1
    mults(2).io.b := b1
    z2 := mults(2).io.p

    val z0 = WireDefault(0.U((2*width).W))
    mults(0).io.a := a0
    mults(0).io.b := b0
    z0 := mults(0).io.p

    // ... and next, the more troublesome sub-product
    val z1 = WireDefault(0.U((2*width).W))
    val (sA, a0a1) = diff(a0, a1)
    val (sB, b1b0) = diff(b1, b0)
    mults(1).io.a := a0a1
    mults(1).io.b := b1b0
    // ... making sure this product is adequately sign-extended
    z1 := Mux(sA ^ sB, -(0.U(width.W) ## mults(1).io.p), 0.U(width.W) ## mults(1).io.p) + z2 + z0

    // Combine and output result
    io.p := (z2 ## 0.U((2*m).W)) + (z1 ## 0.U(m.W)) + z0
  }
}

/** Exact signed/unsigned recursive multiplier
 * 
 * @param width the width of the multiplier
 * @param signed whether the multiplier is for signed numbers (defaults to false)
 * 
 * Converts operands to use the [[RecurMult]] multiplier
 */
class RecursiveMultiplier(width: Int, val signed: Boolean = false) extends Multiplier(width) {
  // Depending on signed, generate an unsigned or signed multiplier
  val mult = Module(new RecurMult(width))
  if (signed) {
    val (sA, sB) = (io.a(width-1), io.b(width-1))
    val ( a,  b) = (Mux(sA, -io.a, io.a), Mux(sB, -io.b, io.b))
    mult.io.a := a
    mult.io.b := b
    io.p := Mux(sA ^ sB, -mult.io.p, mult.io.p)
  } else {
    mult.io.a := io.a
    mult.io.b := io.b
    io.p := mult.io.p
  }
}

/** Exact unsigned alphabet set multiplier
 * 
 * @param width the width of the multiplier (must be an integral factor of 4)
 * 
 * Implementation of the multiplier from Park et al. [2000]
 */
private[approx] class AlphabetSetMult(width: Int) extends Multiplier(width) {
  val stages = width / 4
  require(stages * 4 == width, "width must be an integral factor of 4")

  /** Generate bank of pre-computes
   * 
   * @param x the multiplicand
   * @return vector of eight products [x, 3x, 5x, ..., 15x]
   */
  private[AlphabetSetMult] def precomputes(x: UInt) = {
    require(x.widthKnown && x.getWidth == width, "multiplicand must be w bits")
    val prods = Wire(Vec(8, UInt((width+4).W)))
    prods(0) := x                           // x
    prods(1) := (x ## 0.U(1.W)) +& x        // 2x + x
    prods(2) := (x ## 0.U(2.W)) +& x        // 4x + x
    prods(3) := (x ## 0.U(3.W)) -& x        // 8x - x
    prods(4) := (x ## 0.U(3.W)) +& x        // 8x + x
    prods(5) := (x ## 0.U(3.W)) +& prods(1) // 8x + 3x
    prods(6) := (x ## 0.U(3.W)) +& prods(2) // 8x + 5x
    prods(7) := (x ## 0.U(4.W)) -& x        // 16x - x
    prods
  }

  /** Select right partial product
   * 
   * @param y the 4-bit multiplier segment
   * @param pre the vector of pre-computes
   * @return a partial product
   */
  private[AlphabetSetMult] def select(y: UInt, pre: Vec[UInt]) = {
    require(y.widthKnown && y.getWidth == 4, "width of multiplier must be four bits")
    require(pre.length == 8, "the vector of pre-computes must have eight entries")
    val pprod = WireDefault(0.U((width+8).W))
    when(y =/= 0.U) {
      when(y === BitPat("b???1")) { // odd
        pprod := pre(y(3, 1))
      }.otherwise { // even
        switch(y) {
          is(2.U) {
            pprod := pre(0) ## 0.U(1.W)
          }
          is(4.U) {
            pprod := pre(0) ## 0.U(2.W)
          }
          is(6.U) {
            pprod := pre(1) ## 0.U(1.W)
          }
          is(8.U) {
            pprod := pre(0) ## 0.U(3.W)
          }
          is(10.U) {
            pprod := pre(2) ## 0.U(1.W)
          }
          is(12.U) {
            pprod := pre(1) ## 0.U(2.W)
          }
          is(14.U) {
            pprod := pre(3) ## 0.U(1.W)
          }
        }
      }
    }
    pprod
  }

  // For now, just naively add the partial products
  val pre    = precomputes(io.a)
  val bVec   = io.b.asTypeOf(Vec(stages, UInt(4.W)))
  val pprods = VecInit((0 until stages).map { i => 
    if (i != 0) select(bVec(i), pre) ## 0.U((i * 4).W) else select(bVec(i), pre)
  })
  io.p := pprods.reduceTree(_ + _)
}

/** Exact signed/unsigned alphabet set multiplier
 * 
 * @param width the width of the multiplier
 * @param signed whether the multiplier is for signed numbers (defaults to false)
 * 
 * Converts operands to use the [[AlphabetSetMult]] multiplier
 */
class AlphabetSetMultiplier(width: Int, val signed: Boolean = false) extends Multiplier(width) {
  // Depending on signed, generate an unsigned or signed multiplier
  val mult = Module(new AlphabetSetMult(width))
  if (signed) {
    val (sA, sB) = (io.a(width-1), io.b(width-1))
    val ( a,  b) = (Mux(sA, -io.a, io.a), Mux(sB, -io.b, io.b))
    mult.io.a := a
    mult.io.b := b
    io.p := Mux(sA ^ sB, -mult.io.p, mult.io.p)
  } else {
    mult.io.a := io.a
    mult.io.b := io.b
    io.p := mult.io.p
  }
}
