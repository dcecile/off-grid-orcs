package offGridOrcs

final case class Stock(wood: Int = 0) {
  def +(other: Stock) =
    Stock(wood + other.wood)

  def -(other: Stock) =
    Stock(wood - other.wood)
}

object Stock {
  val Zero = Stock()

  def Wood(wood: Int) = Stock(wood = wood)
}
