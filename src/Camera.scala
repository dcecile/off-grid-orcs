package offGridOrcs

final case class Camera(topLeft: Vec2, velocity: Vec2, zoomOut: ZoomOut) {
  def clamp: Camera =
    this.copy(topLeft = Vec2(
      clampTopOrLeft(topLeft.x),
      clampTopOrLeft(topLeft.y)))

  def clampTopOrLeft(value: Double): Double = {
    (value max 0) min (
      Dimensions.MapSize - (Dimensions.LowRez * zoomOut.value))
  }

  def changeZoomOut(newZoomOut: ZoomOut): Camera = {
    val centerPoint = topLeft + Vec2.One * Dimensions.LowRez * zoomOut.value / 2
    val newCamera = this.copy(
      topLeft = centerPoint - Vec2.One * Dimensions.LowRez * newZoomOut.value / 2,
      zoomOut = newZoomOut)
    newCamera.clamp
  }
}
