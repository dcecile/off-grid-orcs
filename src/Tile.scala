package offGridOrcs

final case class Tile(
  position: Vec2,
  structure: Tile.Structure,
  orc: Option[Reference.Orc],
  building: Option[Reference.Building],
  goal: Option[Reference.Goal],
  stock: Stock
)

object Tile {
  sealed trait Structure
  final case class Trees(shade: TreesShade) extends Structure
  final case class Grass(shade: GrassShade) extends Structure
  final case class Building(stage: BuildingStage) extends Structure

  sealed trait TreesShade
  sealed trait GrassShade
  final case class NoShade() extends TreesShade with GrassShade
  final case class HardHighlight() extends TreesShade
  final case class SoftHighlight() extends TreesShade with GrassShade
  final case class SoftShadow() extends TreesShade with GrassShade
  final case class HardShadow() extends GrassShade

  sealed trait BuildingStage
  final case class Flooring() extends BuildingStage
  final case class Walls() extends BuildingStage
  final case class Roof() extends BuildingStage
  final case class Decal() extends BuildingStage
}
