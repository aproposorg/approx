package approx.addition

import chisel3._

/** Approximate parallel prefix adder base class
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 * 
 * Implementation of the adder from da Rosa et al. [2023]
 */
abstract class AxPPA(width: Int, val approxWidth: Int) extends PPA(width) {
  require(approxWidth <= width, "width of the approximate part must be less than or equal to the total width")
}

/** Approximate parallel prefix adder with Brent-Kung architecture
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 */
class BrentKungAxPPA(width: Int, approxWidth: Int) extends AxPPA(width, approxWidth) {
  val cs = prop(brentKung(genProps.drop(approxWidth)), true.B) ## VecInit(genProps.take(approxWidth).map(_.p)).asUInt
  io.s    := p ^ cs(width-1, 0)
  io.cout := cs(width)
}

/** Approximate parallel prefix adder with Kogge-Stone architecture
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 */
class KoggeStoneAxPPA(width: Int, approxWidth: Int) extends AxPPA(width, approxWidth) {
  val cs = prop(koggeStone(genProps.drop(approxWidth)), true.B) ## VecInit(genProps.take(approxWidth).map(_.p)).asUInt
  io.s    := p ^ cs(width-1, 0)
  io.cout := cs(width)
}

/** Approximate parallel prefix adder with Sklansky architecture
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 */
class SklanskyAxPPA(width: Int, approxWidth: Int) extends AxPPA(width, approxWidth) {
  val cs = prop(sklansky(genProps.drop(approxWidth)), true.B) ## VecInit(genProps.take(approxWidth).map(_.p)).asUInt
  io.s    := p ^ cs(width-1, 0)
  io.cout := cs(width)
}

/** Approximate parallel prefix adder with Ladner-Fischer architecture
 * 
 * @param width the width of the adder
 * @param approxWidth the width of the approximate part (must be less than or equal to the width)
 */
class LadnerFishcerAxPPA(width: Int, approxWidth: Int) extends AxPPA(width, approxWidth) {
  val cs = prop(ladnerFischer(genProps.drop(approxWidth)), true.B) ## VecInit(genProps.take(approxWidth).map(_.p)).asUInt
  io.s    := p ^ cs(width-1, 0)
  io.cout := cs(width)
}
