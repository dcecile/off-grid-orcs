package offGridOrcs

final case class Building(
  id: Reference.Building,
  blueprint: Blueprint,
  positions: Seq[Vec2],
  entrancePosition: Vec2,
  stock: Stock,
  currentOrcs: Int,
  nextOrcTime: Option[Time]
)

object Building {
  def fromBlueprint(id: Reference.Building, topLeft: Vec2, blueprint: Blueprint, world: World): Building = {
    val currentOrcs = if (blueprint.isHeadquarters) { 1 } else { 0 }
    Building(
      id,
      blueprint = blueprint,
      positions = blueprint.buildingPositions
        .map(topLeft + _),
      entrancePosition = topLeft + blueprint.decalPosition,
      stock = Stock.Zero,
      currentOrcs = currentOrcs,
      nextOrcTime = Some(world.currentTime + Timings.OrcHousingSpeed))
  }
}
