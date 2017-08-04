package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom

final case class Loop(model: Model, canvas: SimpleCanvas) {
  def animate(): Unit = {
    updateTiles()
    val shades = new js.typedarray.Float64Array(Dimensions.lowRez.toInt * Dimensions.lowRez.toInt)
    aggregateShades(shades)
    canvas.drawPixels(shades)
    dom.window.requestAnimationFrame(_ => animate)
  }

  def updateTiles(): Unit = {
    for (i <- 0 until model.tiles.length) {
      val tile = model.tiles(i)
      tile.shade = Update.tileShade(tile)
    }
  }

  def aggregateShades(shades: js.typedarray.Float64Array): Unit = {
    val scale = Dimensions.lowRez / Dimensions.screenSize
    for (i <- 0 until model.tiles.length) {
      val tile = model.tiles(i)
      val position = tile.position
      if (position.x < Dimensions.screenSize && position.y < Dimensions.screenSize) {
        val alpha = (1 - tile.shade) * scale * scale
        val i = (position.x * scale).toInt + (position.y * scale).toInt * Dimensions.lowRez.toInt
        shades.update(i, shades(i) + alpha)
      }
    }
  }
}
