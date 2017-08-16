package offGridOrcs

final case class Orc(
  id: Reference.Orc,
  position: Vec2,
  plan: Plan,
  stock: Stock
) {
  def canCarryMore: Boolean =
    stock.wood < 12
  def isCarrying: Boolean =
    stock.wood > 0
}
