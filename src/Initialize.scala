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
      js.Array[Orc](),
      js.Array[Goal]())
    initializeOrc(world)
  }

  def initializeTiles(): js.Array[Tile] = {
    initializeTilePositions()
      .map(position => Tile(
        position,
        Tile.Trees(
          initializeTreesShade(position)),
        orc = None,
        goal = None))
      .toJSArray
  }

  def initializeTilePositions(): Seq[Vec2] = {
    val mapRange = 0 to (Dimensions.MapSize.toInt - 1)
    for (y <- mapRange; x <- mapRange)
      yield Vec2(x.toDouble, y.toDouble)
  }

  def initializeTreesShade(position: Vec2): Shade = {
    def highlight =
      Shade.Highlight()
    def shadow =
      Shade.Shadow()
    (position.x % 32, position.y % 32) match {
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
      case _ => Shade.None()
    }
  }

  def initializeOrc(world: World): World = {
    val position = Vec2.One * (Dimensions.MapSize / 2).floor
    world.execute(Command.InsertOrc(
      Orc(_, position, Plan.Zero)))
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
      Cursor.Build()).clamp
  }
}
