package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom

final case class Loop(model: Model, canvas: SimpleCanvas) {
  val colorBuffer = new js.typedarray.Float64Array(
    Dimensions.LowRez.toInt * Dimensions.LowRez.toInt * 3)

  def send(message: Message): Unit = {
    model.camera = Update.camera(
      model.camera,
      message)
  }

  def animate(): Unit = {
    send(Message.Animate())
    aggregateColors()
    canvas.drawPixels(colorBuffer)
    dom.window.requestAnimationFrame(_ => animate)
  }

  def aggregateColors(): Unit = {
    model.camera.zoomOut match {
      case ZoomOut.OneX() => aggregateColorsOneX()
      case ZoomOut.TwoX() => aggregateColorsTwoX()
    }
  }

  def aggregateColorsOneX(): Unit = {
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = model.camera.topLeft
    val cameraX = cameraTopLeft.x.toInt
    val cameraY = cameraTopLeft.y.toInt
    for (y <- 0 until lowRez) {
      for (x <- 0 until lowRez) {
        val tileX = x + cameraX
        val tileY = y + cameraY
        val tile = model.tiles(tileX + tileY * mapSize)
        val color = View.tileColor(tile)
        val i = (x + y * lowRez) * 3
        colorBuffer.update(i + 0, color.r)
        colorBuffer.update(i + 1, color.g)
        colorBuffer.update(i + 2, color.b)
      }
    }
  }

  def aggregateColorsTwoX(): Unit = {
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = model.camera.topLeft
    val cameraX = cameraTopLeft.x.toInt
    val cameraY = cameraTopLeft.y.toInt
    for (y <- 0 until lowRez) {
      for (x <- 0 until lowRez) {
        val tilesX = x * 2 + cameraX
        val tilesY = y * 2 + cameraY
        val tile00 = model.tiles((tilesX + 0) + (tilesY + 0) * mapSize)
        val color00 = View.tileColor(tile00)
        val tile01 = model.tiles((tilesX + 1) + (tilesY + 0) * mapSize)
        val color01 = View.tileColor(tile01)
        val tile10 = model.tiles((tilesX + 0) + (tilesY + 1) * mapSize)
        val color10 = View.tileColor(tile10)
        val tile11 = model.tiles((tilesX + 1) + (tilesY + 1) * mapSize)
        val color11 = View.tileColor(tile11)
        val i = (x + y * lowRez) * 3
        colorBuffer.update(
          i + 0,
          0.25 * (color00.r + color01.r + color10.r + color11.r))
        colorBuffer.update(
          i + 1,
          0.25 * (color00.g + color01.g + color10.g + color11.g))
        colorBuffer.update(
          i + 2,
          0.25 * (color00.b + color01.b + color10.b + color11.b))
      }
    }
  }
}
