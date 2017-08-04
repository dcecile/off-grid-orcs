package offGridOrcs

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Initialize {
  def model(): Model =
    Model(tiles())

  def tiles(): js.Array[Tile] = {
    val maxIndex = Dimensions.mapSize.toInt - 1
    val positions = for (x <- 0 to maxIndex; y <- 0 to maxIndex) yield Vec2(x.toDouble, y.toDouble)
    def calculateShade(position: Vec2): Double = {
      val center = Vec2(
        (Dimensions.lowRez - 1) / 2,
        (Dimensions.lowRez - 1) / 2)
      val distance = (position - center).length
      val period = 8
      val min = 0.3
      val max = 0.7
      ((min + max) / 2
        + (max - min) / 2
          * Math.sin(distance / period * 2 * Math.PI))
    }
    positions
      .map(position =>
        new Tile(position, calculateShade(position)))
      .toJSArray
  }
}
