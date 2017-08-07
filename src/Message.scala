package offGridOrcs

sealed trait Message
object Message {
  final case class Animate(timeStep: Duration) extends Message
  final case class StartScrollX(speed: Double) extends Message
  final case class StopScrollX() extends Message
  final case class StartScrollY(speed: Double) extends Message
  final case class StopScrollY() extends Message
  final case class ZoomIn() extends Message
  final case class ZoomOut() extends Message
  final case class LeftClick(position: Vec2) extends Message
  final case class MouseMove(position: Vec2) extends Message
  final case class MouseLeave() extends Message
  final case class Reset() extends Message
}
