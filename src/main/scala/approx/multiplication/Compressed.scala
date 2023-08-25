package approx.multiplication

import chisel3._

import approx.addition.{FullAdder, HalfAdder}

import scala.collection.mutable

/** Radix 2 combinational multiplier
 * 
 * @param width the width of the multiplier
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the multiplier of Yang et al. [2019]. 
 * 
 * Only works for unsigned numbers.
 * 
 * @todo Merge with the compressor tree generator.
 */
class CompressedMultiplier(width: Int, val approxWidth: Int) extends Multiplier(width) {
  require(approxWidth <= width, "width of approximate part must be less than the total width")

  /** Generate a 2D bit matrix for radix 2 partial products
   * 
   * @param w the width of the multiplication
   * @return a (non-reduced) 2D bit matrix of radix-2 partial products
   */
  private[CompressedMultiplier] def radix2PProdGen(w: Int) = {
    (0 until w).map { r =>
      (0 until 2*w).map { c =>
        if (r == 0) {
          c <= w
        } else if (r == w-1) {
          c >= w-1
        } else {
          r <= c && c < r+w
        }
      }.toArray
    }.toArray
  }

  private[CompressedMultiplier] type BitPosMatrix = mutable.ArrayBuffer[mutable.ArrayBuffer[Boolean]]

  /** 2-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor2to2 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(2, Bool()))
      val out = Output(Vec(2, Bool()))
    })
    io.out(0) := io.in(0) | io.in(1)
    io.out(1) := io.in(0) & io.in(1)
  }

  /** 3-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor3to2 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(3, Bool()))
      val out = Output(Vec(2, Bool()))
    })
    io.out(0) := io.in.reduceTree(_ | _)
    io.out(1) := (io.in(0) & io.in(1)) | ((io.in(0) | io.in(1)) & io.in(2))
  }

  /** 4-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor4to2 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(4, Bool()))
      val out = Output(Vec(2, Bool()))
    })
    io.out(0) := io.in.reduceTree(_ | _)
    io.out(1) := ((io.in(0) & io.in(1)) | (io.in(2) & io.in(3))) | ((io.in(0) | io.in(1)) & (io.in(2) | io.in(3)))
  }

  /** 5-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor5to3 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(5, Bool()))
      val out = Output(Vec(3, Bool()))
    })
    val g1 = Module(new AppxCompressor3to2)
    val g2 = Module(new AppxCompressor2to2)
    g1.io.in  := io.in.asUInt()(2, 0).asTypeOf(g1.io.in)
    g2.io.in  := io.in.asUInt()(4, 3).asTypeOf(g2.io.in)
    io.out(0) := g1.io.out(0)
    io.out(1) := g1.io.out(1) | g2.io.out(1)
    io.out(2) := g2.io.out(0)
  }

  /** 6-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor6to3 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(6, Bool()))
      val out = Output(Vec(3, Bool()))
    })
    val g1 = Module(new AppxCompressor3to2)
    val g2 = Module(new AppxCompressor3to2)
    g1.io.in  := io.in.asUInt()(2, 0).asTypeOf(g1.io.in)
    g2.io.in  := io.in.asUInt()(5, 3).asTypeOf(g2.io.in)
    io.out(0) := g1.io.out(0)
    io.out(1) := g1.io.out(1) | g2.io.out(1)
    io.out(2) := g2.io.out(0)
  }

  /** 7-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor7to3 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(7, Bool()))
      val out = Output(Vec(3, Bool()))
    })
    val g1 = Module(new AppxCompressor4to2)
    val g2 = Module(new AppxCompressor3to2)
    g1.io.in  := io.in.asUInt()(3, 0).asTypeOf(g1.io.in)
    g2.io.in  := io.in.asUInt()(6, 4).asTypeOf(g2.io.in)
    io.out(0) := g1.io.out(0)
    io.out(1) := g1.io.out(1) | g2.io.out(1)
    io.out(2) := g2.io.out(0)
  }

  /** 8-bit approximate compressor */
  private[CompressedMultiplier] class AppxCompressor8to3 extends Module {
    val io = IO(new Bundle {
      val in  = Input(Vec(8, Bool()))
      val out = Output(Vec(3, Bool()))
    })
    val g1 = Module(new AppxCompressor4to2)
    val g2 = Module(new AppxCompressor4to2)
    g1.io.in  := io.in.asUInt()(3, 0).asTypeOf(g1.io.in)
    g2.io.in  := io.in.asUInt()(7, 4).asTypeOf(g2.io.in)
    io.out(0) := g1.io.out(0)
    io.out(1) := g1.io.out(1) | g2.io.out(1)
    io.out(2) := g2.io.out(0)
  }

  /** Partial product reduction with at most 8:3 compressors
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods eight (potentially, partially reduced) partial products
   * @return the reduced partial products
   * 
   * Both bpos and pprods must be 8 rows of 2*w bits
   */
  private[CompressedMultiplier] def columnReduce8to3(bpos: BitPosMatrix, pprods: Vec[Vec[Bool]]) = {
    require(bpos.length == 8)
    require(pprods.length == 8)
    val bposRes = mutable.ArrayBuffer.fill(8) {
      mutable.ArrayBuffer.fill(bpos.head.length)(false)
    }
    val pprodsRes = WireDefault(VecInit(Seq.fill(8) {
      VecInit(Seq.fill(bpos.head.length)(false.B))
    }))
    (0 until bpos.head.length).foreach { c =>
      val count = (0 until 8).map(r => if (bpos(r)(c)) 1 else 0).reduce(_ + _)
      count match {
        case 1 => // Pass bit through
          bposRes(0)(c) = true
          pprodsRes(0)(c) := pprods(0)(c)
        case 2 => // Pass bits through
          bposRes(0)(c) = true
          bposRes(7)(c) = true
          pprodsRes(0)(c) := pprods(0)(c)
          pprodsRes(7)(c) := pprods(7)(c)
        case 3 => // Pass bits through
          bposRes(0)(c) = true
          bposRes(6)(c) = true
          bposRes(7)(c) = true
          pprodsRes(0)(c) := pprods(0)(c)
          pprodsRes(6)(c) := pprods(6)(c)
          pprodsRes(7)(c) := pprods(7)(c)
        case 4 => // Insert 4:2 compressor
          bposRes(0)(c) = true
          val comp = Module(new AppxCompressor4to2)
          (0 until 4).foreach { r => comp.io.in(r) := pprods(r)(c) }
          pprodsRes(0)(c) := comp.io.out(0)
          if (c+1 < bpos.head.length) {
            bposRes(1)(c+1) = true
            pprodsRes(1)(c+1) := comp.io.out(1)
          }
        case 5 => // Insert 5:3 compressor
          bposRes(0)(c) = true
          val comp = Module(new AppxCompressor5to3)
          (0 until 5).foreach { r => comp.io.in(r) := pprods(r)(c) }
          pprodsRes(0)(c) := comp.io.out(0)
          if (c+1 < bpos.head.length) {
            bposRes(1)(c+1) = true
            pprodsRes(1)(c+1) := comp.io.out(1)
          }
          if (c+2 < bpos.head.length) {
            bposRes(2)(c+2) = true
            pprodsRes(2)(c+2) := comp.io.out(2)
          }
        case 6 => // Insert 6:3 compressor
          bposRes(0)(c) = true
          val comp = Module(new AppxCompressor6to3)
          (0 until 6).foreach { r => comp.io.in(r) := pprods(r)(c) }
          pprodsRes(0)(c) := comp.io.out(0)
          if (c+1 < bpos.head.length) {
            bposRes(1)(c+1) = true
            pprodsRes(1)(c+1) := comp.io.out(1)
          }
          if (c+2 < bpos.head.length) {
            bposRes(2)(c+2) = true
            pprodsRes(2)(c+2) := comp.io.out(2)
          }
        case 7 => // Insert 7:3 compressor
          bposRes(0)(c) = true
          val comp = Module(new AppxCompressor7to3)
          (0 until 7).foreach { r => comp.io.in(r) := pprods(r)(c) }
          pprodsRes(0)(c) := comp.io.out(0)
          if (c+1 < bpos.head.length) {
            bposRes(1)(c+1) = true
            pprodsRes(1)(c+1) := comp.io.out(1)
          }
          if (c+2 < bpos.head.length) {
            bposRes(2)(c+2) = true
            pprodsRes(2)(c+2) := comp.io.out(2)
          }
        case 8 => // Insert 8:3 compressor
          bposRes(0)(c) = true
          val comp = Module(new AppxCompressor8to3)
          (0 until 8).foreach { r => comp.io.in(r) := pprods(r)(c) }
          pprodsRes(0)(c) := comp.io.out(0)
          if (c+1 < bpos.head.length) {
            bposRes(1)(c+1) = true
            pprodsRes(1)(c+1) := comp.io.out(1)
          }
          if (c+2 < bpos.head.length) {
            bposRes(2)(c+2) = true
            pprodsRes(2)(c+2) := comp.io.out(2)
          }
        case _ =>
      }
    }
    (bposRes, pprodsRes)
  }

  /** Partial product reduction with at most 4:2 compressors
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods four (potentially, partially reduced) partial products
   * @return the reduced partial products
   * 
   * Both bpos and pprods must be 4 rows of 2*w bits
   */
  private[CompressedMultiplier] def columnReduce4to2(bpos: BitPosMatrix, pprods: Vec[Vec[Bool]]) = {
    require(bpos.length == 4)
    require(pprods.length == 4)
    val bposRes = mutable.ArrayBuffer.fill(4) {
      mutable.ArrayBuffer.fill(bpos.head.length)(false)
    }
    val pprodsRes = WireDefault(VecInit(Seq.fill(4) {
      VecInit(Seq.fill(bpos.head.length)(false.B))
    }))
    (0 until bpos.head.length).foreach { c =>
      val count = (0 until 4).map { r => if (bpos(r)(c)) 1 else 0 }.sum
      count match {
        case 1 => // Pass bit through
          bposRes(0)(c) = true
          pprodsRes(0)(c) := pprods(0)(c)
        case 2 => // Pass bits through
          bposRes(0)(c) = true
          bposRes(3)(c) = true
          pprodsRes(0)(c) := pprods(0)(c)
          pprodsRes(3)(c) := pprods(3)(c)
        case 3 => // Pass bits through
          bposRes(0)(c) = true
          bposRes(2)(c) = true
          bposRes(3)(c) = true
          pprodsRes(0)(c) := pprods(0)(c)
          pprodsRes(2)(c) := pprods(2)(c)
          pprodsRes(3)(c) := pprods(3)(c)
        case 4 => // Insert 4:2 compressor
          bposRes(0)(c) = true
          val comp = Module(new AppxCompressor4to2)
          (0 until 4).foreach { r => comp.io.in(r) := pprods(r)(c) }
          pprodsRes(0)(c) := comp.io.out(0)
          if (c+1 < bpos.head.length) {
            bposRes(1)(c+1) = true
            pprodsRes(1)(c+1) := comp.io.out(1)
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
  private[CompressedMultiplier] def moveUp(bpos: BitPosMatrix, pprods: Vec[Vec[Bool]]) = {
    val bposRes   = bpos.clone
    val pprodsRes = WireDefault(VecInit(Seq.fill(pprods.length) {
      VecInit(Seq.fill(bpos.head.length) { false.B })
    }))
    var count = 0
    (0 until bpos.head.length).foreach { c =>
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

  /** Iterative partial product reduction with approximate compressors
   * 
   * @param bpos 2D matrix of Booleans representing bit positions in use
   * @param pprods 2D matrix of partial product bits
   * @return the reduced final product
   */
  private[CompressedMultiplier] def columnReduce(bpos: BitPosMatrix, pprods: Vec[Vec[Bool]]) = {
    var bposRes   = bpos.clone
    var pprodsRes = WireDefault(pprods)
    var iter = 1
    while (bposRes.length > 3) {
      iter += 1
      val bposL   = mutable.ArrayBuffer[BitPosMatrix]()
      val pprodsL = mutable.ArrayBuffer[Vec[Vec[Bool]]]()

      // Dynamically select what level of reduction to perform here
      if (bposRes.length >= 8) {
        // Column reduce in chunks of eight rows
        val chunks = bposRes.length / 8
        (0 until chunks).foreach { i =>
          val res = columnReduce8to3(
            bposRes.slice(i*8, (i+1)*8),
            VecInit(pprodsRes.slice(i*8, (i+1)*8))
          )
          bposL   += res._1
          pprodsL += res._2
        }

        // Copy over non-reduced rows
        if (chunks * 8 != bposRes.length) {
          bposL   += bposRes.slice(chunks * 8, bposRes.length)
          pprodsL += VecInit(pprodsRes.slice(chunks * 8, bposRes.length))
        }
      } else {
        // Column reduce in chunks of four rows
        val chunks = bposRes.length / 4
        (0 until chunks).foreach { i =>
          val res = columnReduce4to2(
            bposRes.slice(i*4, (i+1)*4),
            VecInit(pprodsRes.slice(i*4, (i+1)*4))
          )
          bposL   += res._1
          pprodsL += res._2
        }

        // Copy over non-reduced rows
        if (chunks * 4 != bposRes.length) {
          bposL   += bposRes.slice(chunks * 4, bposRes.length)
          pprodsL += VecInit(pprodsRes.slice(chunks * 4, bposRes.length))
        }
      }

      // Bring reduced partial products into a common array
      bposRes = bposL.reduce(_ ++ _)
      val pprodsLRed = Wire(Vec(bposRes.length, Vec(bposRes.head.length, Bool())))
      var r = 0
      (0 until pprodsL.length).foreach { i =>
        pprodsL(i).foreach { l =>
          pprodsLRed(r) := l
          r += 1
        }
      }

      // Move bits up in the array
      val (movBpos, movPprods) = moveUp(bposRes, pprodsLRed)

      // Reduce size of the array
      r = movBpos.length - 1
      while (movBpos(r).forall(!_)) {
        r -= 1
      }
      bposRes   = movBpos.slice(0, r+1)
      pprodsRes = WireDefault(VecInit(movPprods.slice(0, r+1)))
    }

    // Add results (pprodsRes is three rows)
    val sumRed  = WireDefault(VecInit(Seq.fill(2) {
      VecInit(Seq.fill(bposRes.head.length)(false.B))
    }))
    val bposRed = mutable.ArrayBuffer.fill(2){
      mutable.ArrayBuffer.fill(bposRes.head.length)(false)
    }
    (0 until bposRes.head.length).foreach { i =>
      if (bposRes(0)(i) && bposRes(1)(i) && bposRes(2)(i)) {
        bposRed(0)(i) = true
        // Insert full adder
        val fa        = Module(new FullAdder)
        fa.io.x      := pprodsRes(0)(i)
        fa.io.y      := pprodsRes(1)(i)
        fa.io.cin    := pprodsRes(2)(i)
        sumRed(0)(i) := fa.io.s
        if (i+1 < bposRes.head.length) {
          bposRed(1)(i+1) = true
          sumRed(1)(i+1) := fa.io.cout
        }
      } else if (bposRes(0)(i) && bposRes(1)(i)) {
        bposRed(0)(i) = true
        // Insert half adder
        val ha        = Module(new HalfAdder)
        ha.io.x      := pprodsRes(0)(i)
        ha.io.y      := pprodsRes(1)(i)
        sumRed(0)(i) := ha.io.s
        if (i+1 < bposRes.head.length) {
          bposRed(1)(i+1) = true
          sumRed(1)(i+1) := ha.io.cout
        }
      } else if (bposRes(0)(i)) {
        // Pass bit through
        bposRed(0)(i) = true
        sumRed(0)(i) := pprodsRes(0)(i)
      }
    }

    // Add results again (sumRed is two rows)
    val sums  = Wire(Vec(bposRes.head.length, Bool()))
    val couts = Wire(Vec(bposRes.head.length, Bool()))
    sums(0)  := sumRed(0)(0) // always only one bit in use in 
    couts(0) := false.B      // the first column
    (1 until bposRes.head.length).foreach { i =>
      if (bposRed(0)(i) && bposRed(1)(i)) {
        // Insert full adder
        val fa     = Module(new FullAdder)
        fa.io.x   := sumRed(0)(i)
        fa.io.y   := sumRed(1)(i)
        fa.io.cin := couts(i-1)
        sums(i)   := fa.io.s
        couts(i)  := fa.io.cout
      } else {
        // Insert half adder
        val ha    = Module(new HalfAdder)
        ha.io.x  := sumRed(0)(i)
        ha.io.y  := couts(i-1)
        sums(i)  := ha.io.s
        couts(i) := ha.io.cout
      }
    }
    sums.asUInt
  }

  // Generate bit position matrix
  val bposFull = {
    val res = mutable.ArrayBuffer.from(radix2PProdGen(width).map(arr => mutable.ArrayBuffer.from(arr)))
    (0 until approxWidth).foreach { c =>
      (1 until approxWidth).foreach { r =>
        res(r)(c) = false
      }
    }
    res
  }

  // Generate partial products
  val pprodsFull = Wire(Vec(width, Vec(2*width, Bool())))
  (0 until width).foreach { r =>
    val common = VecInit(io.a.asTypeOf(Vec(width, Bool())).map(_ & io.b(r))).asUInt
    if (r == 0) {
      pprodsFull(r) := common.asTypeOf(pprodsFull(r))
    } else {
      pprodsFull(r) := (common ## 0.U(r.W)).asTypeOf(pprodsFull(r))
    }
  }

  // Generate partial product reduction
  val lowerBits = VecInit((0 until approxWidth).map { c =>
    VecInit(pprodsFull.map(_(c))).reduceTree(_ | _)
  }).asUInt
  val pprodsTrunc = VecInit(pprodsFull.map(r => VecInit(r.drop(approxWidth))))
  val bposTrunc   = bposFull.map(_.drop(approxWidth))
  val (movBpos, movPprods) = moveUp(bposTrunc, pprodsTrunc)
  io.p := columnReduce(movBpos, movPprods) ## lowerBits
}
