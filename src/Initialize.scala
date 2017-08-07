package offGridOrcs

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object Initialize {
  def initializeModel(): Model = {
    Model.Title()
  }

  def initializeMapModel(cursorPosition: Vec2): Model.Map = {
    Model.Map(
      initializeWorld(),
      initializeCamera(),
      initializeCursor(cursorPosition))
  }

  def initializeWorld(): World = {
    val world = World(
      Time.Zero,
      initializeTiles(),
      js.Array[Orc]())
    initializeOrc(world)
  }

  def initializeTiles(): js.Array[Tile] = {
    initializeTilePositions()
      .map(position => Tile(
        position,
        Shade.None(),
        None))
      .map(addTileShade)
      .toJSArray
  }

  def initializeTilePositions(): Seq[Vec2] = {
    val mapRange = 0 to (Dimensions.MapSize.toInt - 1)
    for (x <- mapRange; y <- mapRange)
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

  def initializeOrc(world: World): World = {
    world.execute(Command.InsertOrc(
      Vec2.One * (Dimensions.MapSize / 2).floor,
      Plan.idle(Time.Zero)))
  }

  def initializeCamera(): Camera = {
    Camera(
      topLeft = Vec2.One
        * ((Dimensions.MapSize - Dimensions.LowRez) / 2).floor,
      velocity = Vec2.Zero,
      zoomOut = ZoomOut.OneX())
  }

  def initializeCursor(position: Vec2): Cursor = {
    Cursor(
      Some(position),
      Cursor.Inspect()).clamp
  }
}
