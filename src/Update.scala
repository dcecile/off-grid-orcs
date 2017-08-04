package offGridOrcs

object Update {
  def camera(camera: Camera, message: Message): Camera = {
    message match {
      case Message.Animate() =>
        if (camera.velocity != Vec2.zero) {
          camera
            .copy(topLeft = (camera.topLeft + camera.velocity))
            .clamp()
        } else {
          camera
        }
      case Message.StartScrollX(speed) =>
        camera.copy(velocity =
          camera.velocity.copy(x = speed))
      case Message.StopScrollX() =>
        camera.copy(velocity =
          camera.velocity.copy(x = 0))
      case Message.StartScrollY(speed) =>
        camera.copy(velocity =
          camera.velocity.copy(y = speed))
      case Message.StopScrollY() =>
        camera.copy(velocity =
          camera.velocity.copy(y = 0))
    }
  }

  def tileShade(tile: Tile): Double = {
    (tile.shade + 0.01) % 1
  }
}
