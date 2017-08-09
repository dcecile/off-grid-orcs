package offGridOrcs

final case class Camera(topLeft: Vec2, velocity: Vec2, zoomOut: ZoomOut) {
  def clamp: Camera =
    this.copy(topLeft = topLeft.clamp(
      Vec2.Zero,
      Vec2.One * (Dimensions.MapSize - Dimensions.LowRez * zoomOut.value)))

  def changeZoomOut(newZoomOut: ZoomOut): Camera = {
    val centerPoint = topLeft + Vec2.One * Dimensions.LowRez * zoomOut.value / 2
    this.copy(
      topLeft = centerPoint - Vec2.One * Dimensions.LowRez * newZoomOut.value / 2,
      zoomOut = newZoomOut).clamp
  }
}
