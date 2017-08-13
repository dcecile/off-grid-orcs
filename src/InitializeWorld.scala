package offGridOrcs

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object InitializeWorld {
  def initialize(): World = {
    val world = World(
      Time.Zero,
      initializeTiles(),
      js.Array[Orc](),
      js.Array[Option[Goal]]())
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

  def initializeTreesShade(position: Vec2): Tile.TreesShade = {
    def highlight =
      Tile.SoftHighlight()
    def shadow =
      Tile.SoftShadow()
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
      case _ => Tile.NoShade()
    }
  }

  def initializeGrassShade(position: Vec2): Tile.GrassShade = {
    def highlight =
      Tile.SoftHighlight()
    def shadow =
      Tile.SoftShadow()
    (position.x % 16, position.y % 16) match {
      // 0, 0
      case (0x00, 0x02) => shadow
      case (0x01, 0x00) => highlight
      case (0x03, 0x04) => shadow
      case (0x04, 0x03) => highlight
      case (0x07, 0x01) => shadow
      case (0x00, 0x07) => highlight
      // 0, 1
      case (0x02, 0x0a) => shadow
      case (0x06, 0x0f) => highlight
      case (0x03, 0x0c) => shadow
      case (0x04, 0x0d) => highlight
      case (0x05, 0x0b) => shadow
      case (0x03, 0x08) => highlight
      // 1, 0
      case (0x09, 0x03) => shadow
      case (0x0c, 0x07) => highlight
      case (0x07, 0x05) => shadow
      case (0x06, 0x07) => highlight
      case (0x0d, 0x02) => shadow
      case (0x0f, 0x01) => highlight
      // 1, 1
      case (0x08, 0x09) => shadow
      case (0x09, 0x0b) => highlight
      case (0x0a, 0x0f) => shadow
      case (0x0b, 0x0d) => shadow
      case (0x0e, 0x0c) => shadow
      case (0x0c, 0x0a) => highlight
      case (0x0f, 0x08) => shadow
      case (0x0d, 0x0e) => highlight
      // No shade
      case _ => Tile.NoShade()
    }
  }

  def initializeOrc(world: World): World = {
    val position = Vec2.One * (Dimensions.MapSize / 2).floor
    world.execute(Seq(Command.InsertOrc(
      Orc(_, position, Plan.Zero))))
  }
}
