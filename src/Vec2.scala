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

  def spriteTopLeft(size: Int) =
    (this - Vec2.One * (size.toDouble / 2 - 1)).floor

  def clamp(topLeft: Vec2, size: Vec2): Vec2 = {
    val bottomRight = topLeft + size
    Vec2(
      (x max (topLeft.x)) min (bottomRight.x),
      (y max (topLeft.y)) min (bottomRight.y))
  }
}

object Vec2 {
  val Zero = Vec2(0, 0)
  val One = Vec2(1, 1)
}
