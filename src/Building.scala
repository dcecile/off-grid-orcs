package offGridOrcs

final case class Building(
  id: Reference.Building,
  name: String,
  positions: Seq[Vec2],
  entrance: Vec2,
  stock: Stock
)

object Building {
  def fromBlueprint(id: Reference.Building, topLeft: Vec2, blueprint: Blueprint): Building = {
    Building(
      id,
      name = blueprint.name,
      positions = blueprint.buildingPositions
        .map(topLeft + _),
      entrance = topLeft + blueprint.decalPosition,
      stock = Stock.Zero)
  }
}
