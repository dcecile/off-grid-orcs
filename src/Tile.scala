package offGridOrcs

final case class Tile(position: Vec2, structure: Tile.Structure, orc: Option[Reference.Orc], goal: Option[Reference.Goal])

object Tile {
  sealed trait Structure
  final case class Trees(shade: Shade) extends Structure
  final case class Grass() extends Structure
}
