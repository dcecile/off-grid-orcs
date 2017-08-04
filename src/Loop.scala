package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom

final case class Loop(model: Model, canvas: SimpleCanvas) {
  def send(message: Message): Unit = {
    model.camera = Update.camera(
      model.camera,
      message)
  }

  def animate(): Unit = {
    send(Message.Animate())
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
      val screenPosition = (tile.position - model.camera.topLeft) * scale
      val x = screenPosition.x
      val y = screenPosition.y
      if (0 <= x && x < Dimensions.lowRez && 0 <= y && y < Dimensions.lowRez) {
        val alpha = (1 - tile.shade) * scale * scale
        val i = x.toInt + y.toInt * Dimensions.lowRez.toInt
        shades.update(i, shades(i) + alpha)
      }
    }
  }
}
