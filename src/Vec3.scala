package offGridOrcs

final case class Vec3(x: Double, y: Double, z: Double) {
  def r = x

  def g = y

  def b = z

  def +(other: Vec3) =
    Vec3(x + other.x, y + other.y, z + other.z)

  def -(other: Vec3) =
    Vec3(x - other.x, y - other.y, z - other.z)

  def *(other: Vec3) =
    Vec3(x * other.x, y * other.y, z * other.z)

  def *(scale: Double) =
    Vec3(x * scale, y * scale, z * scale)

  def /(scale: Double) =
    Vec3(x / scale, y / scale, z / scale)

  def dot(other: Vec3) =
    x * other.x + y * other.y + z * other.z

  def lengthSquared =
    this dot this

  def length =
    Math.sqrt(lengthSquared)

  def lighten(value: Double) = {
    this * (1 + value)
  }

  def darken(value: Double) = {
    this * (1 - value)
  }
}

object Vec3 {
  val zero = Vec3(0, 0, 0)
  val one = Vec3(1, 1, 1)
  def hexRGB(hexCode: Int): Vec3 = {
    val r = (hexCode & 0xFF0000) >>> 16
    val g = (hexCode & 0x00FF00) >>> 8
    val b = (hexCode & 0x0000FF) >>> 0
    Vec3(
      r.toDouble / 255,
      g.toDouble / 255,
      b.toDouble / 255)
  }
}
