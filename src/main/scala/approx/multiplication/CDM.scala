package approx.multiplication

import chisel3._

/** Carry disregard multiplier
 *
 * @param width the width of the multiplier
 * @param approxWidth the number of least significant columns that disregard carries
 *
 * Implementation of the multiplier of Amirafshar et al. [2023]
 *
 * The design splits the multiplier into chunks of four rows (4-by-n-bit
 * multipliers) and sums the results using carry-lookahead style addition.
 *
 * Only works for unsigned numbers.
 */
class CDM(width: Int, approxWidth: Int) extends Multiplier(width, width) {
  require(width > 0 && approxWidth >= 0 && approxWidth <= 2*width - 1,
    "width must be positive and approxWidth must be between 0 and 2*width-1")

  val padWidth = ((width + 3) / 4) * 4

  val paddedA = io.a.pad(padWidth)
  val paddedB = io.b.pad(padWidth)

  val fullResult = if (padWidth == 4) {
    abstract class PartialProductCell {
      def apply(sumIn: Bool, pp: Bool, cin: Bool): (Bool, Bool) // (sum, cout)
    }

    object Cell50 extends PartialProductCell { // Exact full adder cell
      def apply(sumIn: Bool, pp: Bool, cin: Bool): (Bool, Bool) = {
        val sum  =  sumIn ^ pp ^ cin
        val cout = (sumIn & pp) | (cin & (sumIn | pp))
        (sum, cout)
      }
    }

    object Cell51 extends PartialProductCell { // Carry disregard cell - no cin/cout
      def apply(sumIn: Bool, pp: Bool, cin: Bool): (Bool, Bool) = {
        val sum  = sumIn ^ pp // Disregard carry
        val cout = false.B // No carry output
        (sum, cout)
      }
    }

    object Cell52 extends PartialProductCell { // Half-adder based - no cin, has cout
      def apply(sumIn: Bool, pp: Bool, cin: Bool): (Bool, Bool) = {
        val sum  = sumIn ^ pp // Disregard cin
        val cout = sumIn & pp
        (sum, cout)
      }
    }

    object Cell53 extends PartialProductCell { // Full-adder based with two pp - no cin, has cout
      def apply(sumIn: Bool, pp: Bool, cin: Bool): (Bool, Bool) = {
        val sum  = sumIn ^ pp // Disregard cin
        val cout = sumIn & pp
        (sum, cout)
      }
    }

    // Build the partial product bits
    val ppBits = Wire(Vec(padWidth, Vec(2*padWidth, Bool())))
    for (i <- 0 until padWidth) {
      for (j <- 0 until 2*padWidth) {
        if (j >= i && j < i + padWidth)
          ppBits(i)(j) := paddedA(i) & paddedB(j - i)
        else
          ppBits(i)(j) := false.B
      }
    }

    // Vecs to hold sum and carry for each cell
    val sums  = Wire(Vec(padWidth, Vec(2*padWidth, Bool())))
    val couts = Wire(Vec(padWidth, Vec(2*padWidth, Bool())))

    // First row: just partial products, no cells
    for (j <- 0 until 2*padWidth) {
      sums(0)(j)  := ppBits(0)(j)
      couts(0)(j) := false.B
    }

    // Subsequent rows: use cells
    for (i <- 1 until padWidth) {
      for (j <- 0 until 2*padWidth) {
        // Cell selection based on column position
        val cell = if (j < approxWidth) {
          Cell51
        } else if (j == approxWidth) {
          Cell52
        } else if (j == approxWidth + 1) {
          Cell53
        } else {
          Cell50
        }

        val cin = if (j == 0) false.B else couts(i)(j-1)
        val (sum, cout) = cell(sums(i-1)(j), ppBits(i)(j), cin)
        sums(i)(j)  := sum
        couts(i)(j) := cout
      }
    }

    // Final result is the last row sums
    sums(padWidth-1).asUInt
  } else {
    // Hierarchical case: split into four (padWidth/2)-bit multipliers and sum results
    val half = padWidth / 2

    // Adjust disregard widths for sub-multipliers based on their contribution to final bits
    // Clamp to valid range [0, 2*half-1] for each sub-multiplier
    val aWLL = math.min(approxWidth, 2*half - 1)
    val aWLH = math.min(math.max(0, approxWidth - half), 2*half - 1)
    val aWHL = math.min(math.max(0, approxWidth - half), 2*half - 1)
    val aWHH = math.min(math.max(0, approxWidth - padWidth), 2*half - 1)

    // Create four sub-multipliers
    val ll = Module(new CDM(half, aWLL))
    ll.io.a := paddedA(half-1, 0)
    ll.io.b := paddedB(half-1, 0)

    val lh = Module(new CDM(half, aWLH))
    lh.io.a := paddedA(half-1, 0)
    lh.io.b := paddedB(padWidth-1, half)

    val hl = Module(new CDM(half, aWHL))
    hl.io.a := paddedA(padWidth-1, half)
    hl.io.b := paddedB(half-1, 0)

    val hh = Module(new CDM(half, aWHH))
    hh.io.a := paddedA(padWidth-1, half)
    hh.io.b := paddedB(padWidth-1, half)

    // Sum the results with appropriate shifts
    ll.io.p + (lh.io.p ## 0.U(half.W)) + (hl.io.p ## 0.U(half.W)) + (hh.io.p ## 0.U(padWidth.W))
  }

  io.p := fullResult(2*width - 1, 0)
}
