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

    draw()
    if (!isAnimationFrameRequested) {
      previousJSTimestamp = None
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
      case inspectionModel: Model.Inspection =>
        aggregateColors(inspectionModel.mapModel)
        overlaySprites(inspectionModel)
        canvas.drawPixels(colorBuffer)
        requestAnimationFrame()
    }
  }

  def requestAnimationFrame(): Unit = {
    if (!isAnimationFrameRequested) {
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
  }

  def aggregateColors(model: Model.Map): Unit = {
    model.camera.zoomOut match {
      case ZoomOut.OneX() =>
        aggregateColorsOneX(model)
      case ZoomOut.TwoX() =>
        aggregateColorsTwoX(model)
    }
  }

  def aggregateColorsOneX(model: Model.Map): Unit = {
    val tiles = model.world.tiles
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = model.camera.topLeft
    val cameraX = cameraTopLeft.x.toInt
    val cameraY = cameraTopLeft.y.toInt
    for (y <- 0 until lowRez) {
      for (x <- 0 until lowRez) {
        val tileX = x + cameraX
        val tileY = y + cameraY
        val tile = tiles(tileX + tileY * mapSize)
        val color = View.viewTileColor(model, tile)
        val i = (x + y * lowRez) * 3
        colorBuffer.update(i + 0, color.r)
        colorBuffer.update(i + 1, color.g)
        colorBuffer.update(i + 2, color.b)
      }
    }
  }

  def aggregateColorsTwoX(model: Model.Map): Unit = {
    val tiles = model.world.tiles
    val lowRez = Dimensions.LowRez.toInt
    val mapSize = Dimensions.MapSize.toInt
    val cameraTopLeft = model.camera.topLeft
    val cameraX = cameraTopLeft.x.toInt
    val cameraY = cameraTopLeft.y.toInt
    for (y <- 0 until lowRez) {
      for (x <- 0 until lowRez) {
        val tilesX = x * 2 + cameraX
        val tilesY = y * 2 + cameraY
        val tile00 = tiles((tilesX + 0) + (tilesY + 0) * mapSize)
        val color00 = View.viewTileColor(model, tile00)
        val tile01 = tiles((tilesX + 1) + (tilesY + 0) * mapSize)
        val color01 = View.viewTileColor(model, tile01)
        val tile10 = tiles((tilesX + 0) + (tilesY + 1) * mapSize)
        val color10 = View.viewTileColor(model, tile10)
        val tile11 = tiles((tilesX + 1) + (tilesY + 1) * mapSize)
        val color11 = View.viewTileColor(model, tile11)
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

  def overlaySprites(model: Model.Map): Unit = {
    overlaySprites(
      View.viewHeadsUpDisplay(model))
  }

  def overlaySprites(model: Model.Inspection): Unit = {
    overlaySprites(
      View.viewInspectionScreen(model))
  }

  def overlaySprites(sprites: Seq[Sprite]): Unit = {
    val lowRez = Dimensions.LowRez.toInt
    for (sprite <- sprites) {
      val bitmap = sprite.bitmap
      val offsetX = sprite.position.x.toInt
      val offsetY = sprite.position.y.toInt
      for (y <- 0 until bitmap.size) {
        for (x <- 0 until bitmap.size) {
          val i = (x + y * bitmap.size) * 3
          val j = (x + offsetX + (y + offsetY) * lowRez) * 3
          colorBuffer.update(
            j + 0,
            colorBuffer(j + 0) + bitmap(i + 0) * sprite.alpha)
          colorBuffer.update(
            j + 1,
            colorBuffer(j + 1) + bitmap(i + 1) * sprite.alpha)
          colorBuffer.update(
            j + 2,
            colorBuffer(j + 2) + bitmap(i + 2) * sprite.alpha)
        }
      }
    }
  }
}
