package offGridOrcs

final case class Camera(topLeft: Vec2, velocity: Vec2) {
  def clamp(): Camera =
    this.copy(topLeft = Vec2(
      clampTopOrLeft(topLeft.x),
      clampTopOrLeft(topLeft.y)))

  def clampTopOrLeft(value: Double): Double = {
    (value max 0) min (Dimensions.mapSize - Dimensions.screenSize)
  }
}
