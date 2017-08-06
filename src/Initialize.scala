package offGridOrcs

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Initialize {
  def initializeModel(): Model =
    new Model(
      initializeTiles(),
      initializeTitleState())

  def initializeTiles(): js.Array[Tile] = {
    initializeTilePositions()
      .map(position => Tile(position, Shade.None()))
      .map(addTileShade)
      .toJSArray
  }

  def initializeTilePositions(): Seq[Vec2] = {
    val maxIndex = Dimensions.MapSize.toInt - 1
    for (x <- 0 to maxIndex; y <- 0 to maxIndex)
      yield Vec2(x.toDouble, y.toDouble)
  }

  def addTileShade(tile: Tile): Tile = {
    def highlight =
      tile.copy(shade = Shade.Highlight())
    def shadow =
      tile.copy(shade = Shade.Shadow())
    (tile.position.x % 32, tile.position.y % 32) match {
      // 0, 0
      case (0x00, 0x03) => shadow
      case (0x09, 0x05) => highlight
      case (0x05, 0x0a) => shadow
      case (0x0c, 0x0f) => highlight
      // 0, 1
      case (0x0f, 0x12) => shadow
      case (0x04, 0x10) => highlight
      case (0x0c, 0x19) => shadow
      case (0x0e, 0x1e) => highlight
      // 1, 0
      case (0x12, 0x04) => shadow
      case (0x1c, 0x08) => highlight
      case (0x19, 0x0c) => shadow
      case (0x15, 0x0e) => highlight
      // 1, 1
      case (0x19, 0x1f) => shadow
      case (0x12, 0x17) => highlight
      case (0x17, 0x1a) => shadow
      case (0x1a, 0x10) => highlight
      // No shade
      case _ => tile
    }
  }

  def initializeTitleState(): UIState.Title = {
    UIState.Title()
  }

  def initializeMapState(): UIState.Map = {
    UIState.Map(
      initializeCamera())
  }

  def initializeCamera(): Camera = {
    Camera(
      topLeft = Vec2.zero,
      velocity = Vec2.zero,
      zoomOut = ZoomOut.OneX())
  }
}
