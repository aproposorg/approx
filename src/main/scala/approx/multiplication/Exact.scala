package approx.multiplication

import chisel3._
import chisel3.util.{BitPat, is, log2Up, switch}

import approx.addition.{HalfAdder, FullAdder}
import approx.multiplication.comptree._

/** 2:2 compressor */
class Compressor2to2 extends C2to2 {
  val add = Module(new HalfAdder)
  add.io.x := io.x1
  add.io.y := io.x2
  io.s    := add.io.s
  io.cout := add.io.cout
}

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
 * @param aSigned whether the first operand is signed (defaults to false)
 * @param bSigned whether the second operand is signed (defaults to false)
 * @param targetDevice a string indicating the target device (defaults to "",
 *                     meaning ASIC)
 * @param approx the targeted approximation style (defaults to no approximation)
 * 
 * Makes use of the compressor tree generator to add partial products.
 */
class Radix2Multiplier(width: Int, aSigned: Boolean = false, bSigned: Boolean = false,
                       targetDevice: String = "", approx: Approximation = NoApproximation())
  extends Multiplier(width) {
  /** Compute the number of partial product bits in a particular
   * column of the tree
   * 
   * @param col the column of the tree
   * @param aW the width of the first operand
   * @param bW the width of the second operand
   * @param midLow the position of the least significant sign-extension
   *               constant bit
   * @param midHigh the position of the most significant sign-extension
   *                constant bit
   * @param upper the position of the most significant partial product bit
   * @return the number of bits in column `col`
   */
  private[Radix2Multiplier] def dotCount(col: Int, aW: Int, bW: Int, midLow: Int, midHigh: Int, upper: Int): Int = {
    if (midLow > col) (col + 1)
    else if (midHigh >= col && col >= midLow) scala.math.min(aW, bW)
    else if (upper > col) (aW + bW - col - 1)
    else 0
  }

  /** Compute the index of the least significant row from which to
   * take partial product bits in a particular column of the tree
   * 
   * @param col the column of the tree
   * @param bW the width of the second operand
   * @return the least significant row index in column `col`
   */
  private[Radix2Multiplier] def lsCol(col: Int, bW: Int): Int = if (bW > col) 0 else col - bW + 1

  // Depending on aSigned and bSigned, generate an unsigned or signed multiplier
  if (aSigned || bSigned) {
    // ... at least one operand is signed
    // Extend the unsigned operand by one bit
    val (aW, opA) = if (aSigned) (width, io.a) else (width+1, false.B ## io.a)
    val (bW, opB) = if (bSigned) (width, io.b) else (width+1, false.B ## io.b)

    // Create all the partial products
    val pprods = (0 until aW-1).map { i =>
      ~(opB(bW-1) & opA(i)) ## VecInit(opB.asBools.dropRight(1).map(_ & opA(i))).asUInt
    } :+ ((opB(bW-1) & opA(aW-1)) ## ~VecInit(opB.asBools.dropRight(1).map(_ & opA(aW-1))).asUInt)

    // Compute the positions of the sign-extension constants
    val midLow  = scala.math.min(aW, bW) - 1
    val midHigh = scala.math.max(aW, bW) - 1
    val upper   = aW + bW - 1

    // Instantiate a compressor tree and input the bits
    // (incl. sign-extension constant)
    val sig  = new MultSignature(aW, bW, true, true)
    val comp = Module(CompressorTree(sig, targetDevice=targetDevice, approx=approx))
    val ins  = Wire(Vec(sig.count, Bool()))
    var offset = 0
    (0 until sig.outW).foreach { col =>
      // Add the partial product bits
      val low  = lsCol(col, bW)
      val high = low + dotCount(col, aW, bW, midLow, midHigh, upper)
      (low until high).foreach { row =>
        ins(offset) := pprods(row)(col - row)
        offset += 1
      }

      // Add the sign-extension bits
      if (col == midLow) {
        ins(offset) := true.B
        offset += 1
      }
      if (col == midHigh) {
        ins(offset) := true.B
        offset += 1
      }
      if (col == upper) {
        ins(offset) := true.B
        offset += 1
      }
    }
    comp.io.in := ins.asUInt
    io.p := comp.io.out
  } else {
    // ... both operands are unsigned
    val (aW, opA) = (width, io.a)
    val (bW, opB) = (width, io.b)

    // Create all the partial products
    val pprods = (0 until aW).map { i => VecInit(opB.asBools.map(_ & opA(i))).asUInt }

    // Compute the positions of the sign-extension constants
    val midLow  = scala.math.min(aW, bW) - 1
    val midHigh = scala.math.max(aW, bW) - 1
    val upper   = aW + bW - 1

    // Instantiate a compressor tree and input the bits
    val sig  = new MultSignature(aW, bW, false, false)
    val comp = Module(CompressorTree(sig, targetDevice=targetDevice, approx=approx))
    val ins  = Wire(Vec(sig.count, Bool()))
    var offset = 0
    (0 until sig.outW).foreach { col =>
      // Add the partial product bits
      val low  = lsCol(col, bW)
      val high = low + dotCount(col, aW, bW, midLow, midHigh, upper)
      (low until high).foreach { row =>
        ins(offset) := pprods(row)(col - row)
        offset += 1
      }
    }
    comp.io.in := ins.asUInt
    io.p := comp.io.out
  }
}

/** Exact signed/unsigned recursive multiplier
 * 
 * @param width the width of the multiplier
 * @param signed whether the multiplier is for signed numbers (defaults to false)
 */
class RecursiveMultiplier(width: Int, val signed: Boolean = false) extends Multiplier(width) {
  /** Exact unsigned recursive multiplier
   * 
   * @param width the width of the multiplier
   * 
   * Implementation of Karatsuba's algorithm cf. Danysh and Swartzlander [1998]
   */
  private[RecursiveMultiplier] class Mult(width: Int) extends Multiplier(width) {
    /** Calculate absolute difference and sign of two operands
     * 
     * @param x0 the first operand
     * @param x1 the second operand
     * @return a pair of (sign, absolute difference)
     */
    private[Mult] def diff(x0: UInt, x1: UInt) = {
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
      val mults    = Array.fill(3) { Module(new Mult(m)) }

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

  // Depending on signed, generate an unsigned or signed multiplier
  val mult = Module(new Mult(width))
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

/** Exact signed/unsigned alphabet set multiplier
 * 
 * @param width the width of the multiplier
 * @param signed whether the multiplier is for signed numbers (defaults to false)
 */
class AlphabetSetMultiplier(width: Int, val signed: Boolean = false) extends Multiplier(width) {
  /** Exact unsigned alphabet set multiplier
   * 
   * @param width the width of the multiplier (must be an integral factor of 4)
   * 
   * Implementation of the multiplier from Park et al. [2000]
   */
  private[AlphabetSetMultiplier] class Mult(width: Int) extends Multiplier(width) {
    val stages = width / 4
    require(stages * 4 == width, "width must be an integral factor of 4")

    /** Generate bank of pre-computes
     * 
     * @param x the multiplicand
     * @return vector of eight products [x, 3x, 5x, ..., 15x]
     */
    private[Mult] def precomputes(x: UInt) = {
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
    private[Mult] def select(y: UInt, pre: Vec[UInt]) = {
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

  // Depending on signed, generate an unsigned or signed multiplier
  val mult = Module(new Mult(width))
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
