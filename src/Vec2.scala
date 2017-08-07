package offGridOrcs

final case class Vec2(x: Double, y: Double) {
  def +(other: Vec2) =
    Vec2(x + other.x, y + other.y)

  def -(other: Vec2) =
    Vec2(x - other.x, y - other.y)

  def *(other: Vec2) =
    Vec2(x * other.x, y * other.y)

  def *(scale: Double) =
    Vec2(x * scale, y * scale)

  def /(scale: Double) =
    Vec2(x / scale, y / scale)

  def dot(other: Vec2) =
    x * other.x + y * other.y

  def lengthSquared =
    this dot this

  def length =
    Math.sqrt(lengthSquared)

  def floor =
    Vec2(x.floor, y.floor)
}

object Vec2 {
  val Zero = Vec2(0, 0)
  val One = Vec2(1, 1)
}
