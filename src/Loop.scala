package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom

final class Loop(var model: Model, val canvas: SimpleCanvas) {
  var previousJSTimestamp: Option[Double] = None

  var isAnimationFrameRequested = false

  val colorBuffer = new js.typedarray.Float64Array(
    (Dimensions.LowRez * Dimensions.LowRez * 3).toInt)

  def send(message: Message): Unit = {
    model = Update.updateModel(model, message)

    if (!isAnimationFrameRequested) {
      draw()
      if (!isAnimationFrameRequested) {
        previousJSTimestamp = None
      }
    }
  }

  def draw(): Unit = {
    model match {
      case _: Model.Title =>
        canvas.drawTitle()
      case mapModel: Model.Map =>
        aggregateColors(mapModel)
        overlaySprites(mapModel)
        canvas.drawPixels(colorBuffer)
        requestAnimationFrame()
    }
  }

  def requestAnimationFrame(): Unit = {
    dom.window.requestAnimationFrame({ currentJSTimestamp =>
      val duration = previousJSTimestamp match {
        case None =>
          Duration.Zero
        case Some(previousJSTimestamp) =>
          Duration.fromJSTimestamps(previousJSTimestamp, currentJSTimestamp)
      }
      isAnimationFrameRequested = false
      send(Message.Animate(duration))
      previousJSTimestamp = Some(currentJSTimestamp)
    })
    isAnimationFrameRequested = true
  }

  def aggregateColors(mapModel: Model.Map): Unit = {
    mapModel.camera.zoomOut match {
      case ZoomOut.OneX() =>
        aggregateColorsOneX(mapModel)
      case ZoomOut.TwoX() =>
        aggregateColorsTwoX(mapModel)
    }
  }

  def aggregateColorsOneX(mapModel: Model.Map): Unit = {
    val tiles = mapModel.world.tiles
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = mapModel.camera.topLeft
    val cameraX = cameraTopLeft.x.toInt
    val cameraY = cameraTopLeft.y.toInt
    for (y <- 0 until lowRez) {
      for (x <- 0 until lowRez) {
        val tileX = x + cameraX
        val tileY = y + cameraY
        val tile = tiles(tileX + tileY * mapSize)
        val color = View.viewTileColor(tile)
        val i = (x + y * lowRez) * 3
        colorBuffer.update(i + 0, color.r)
        colorBuffer.update(i + 1, color.g)
        colorBuffer.update(i + 2, color.b)
      }
    }
  }

  def aggregateColorsTwoX(mapModel: Model.Map): Unit = {
    val tiles = mapModel.world.tiles
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = mapModel.camera.topLeft
    val cameraX = cameraTopLeft.x.toInt
    val cameraY = cameraTopLeft.y.toInt
    for (y <- 0 until lowRez) {
      for (x <- 0 until lowRez) {
        val tilesX = x * 2 + cameraX
        val tilesY = y * 2 + cameraY
        val tile00 = tiles((tilesX + 0) + (tilesY + 0) * mapSize)
        val color00 = View.viewTileColor(tile00)
        val tile01 = tiles((tilesX + 1) + (tilesY + 0) * mapSize)
        val color01 = View.viewTileColor(tile01)
        val tile10 = tiles((tilesX + 0) + (tilesY + 1) * mapSize)
        val color10 = View.viewTileColor(tile10)
        val tile11 = tiles((tilesX + 1) + (tilesY + 1) * mapSize)
        val color11 = View.viewTileColor(tile11)
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

  def overlaySprites(mapModel: Model.Map): Unit = {
    val lowRez = Dimensions.LowRez.toInt
    for (sprite <- View.viewSprites(mapModel)) {
      val buffer = sprite.buffer
      val offsetX = sprite.position.x.toInt
      val offsetY = sprite.position.y.toInt
      for (y <- 0 until buffer.size) {
        for (x <- 0 until buffer.size) {
          val i = (x + y * buffer.size) * 3
          val j = (x + offsetX + (y + offsetY) * lowRez) * 3
          colorBuffer.update(
            j + 0,
            colorBuffer(j + 0) + buffer(i + 0) * sprite.alpha)
          colorBuffer.update(
            j + 1,
            colorBuffer(j + 1) + buffer(i + 1) * sprite.alpha)
          colorBuffer.update(
            j + 2,
            colorBuffer(j + 2) + buffer(i + 2) * sprite.alpha)
        }
      }
    }
  }
}
