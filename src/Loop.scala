package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom

final class Loop(var model: Model, val canvas: SimpleCanvas) {
  var isAnimationFrameRequested = false

  val colorBuffer = new js.typedarray.Float64Array(
    (Dimensions.LowRez * Dimensions.LowRez * 3).toInt)

  def send(message: Message): Unit = {
    model = Update.updateModel(model, message)

    if (!isAnimationFrameRequested) {
      draw()
    }
  }

  def draw(): Unit = {
    model.uiState match {
      case _: UIState.Title =>
        canvas.drawTitle()
      case mapState: UIState.Map =>
        aggregateColors(mapState.camera)
        canvas.drawPixels(colorBuffer)
        dom.window.requestAnimationFrame({ _ =>
          isAnimationFrameRequested = false
          send(Message.Animate())
        })
        isAnimationFrameRequested = true
    }
  }

  def aggregateColors(camera: Camera): Unit = {
    camera.zoomOut match {
      case ZoomOut.OneX() =>
        aggregateColorsOneX(camera)
      case ZoomOut.TwoX() =>
        aggregateColorsTwoX(camera)
    }
  }

  def aggregateColorsOneX(camera: Camera): Unit = {
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = camera.topLeft
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

  def aggregateColorsTwoX(camera: Camera): Unit = {
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = camera.topLeft
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
