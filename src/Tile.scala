package offGridOrcs

final case class Tile(position: Vec2, structure: Tile.Structure, orc: Option[Reference.Orc], goal: Option[Reference.Goal])

object Tile {
  sealed trait Structure
  final case class Trees(shade: TreesShade) extends Structure
  final case class Grass(shade: GrassShade) extends Structure

  sealed trait TreesShade
  sealed trait GrassShade
  final case class NoShade() extends TreesShade with GrassShade
  final case class HardHighlight() extends TreesShade
  final case class SoftHighlight() extends TreesShade with GrassShade
  final case class SoftShadow() extends TreesShade with GrassShade
  final case class HardShadow() extends GrassShade
}
