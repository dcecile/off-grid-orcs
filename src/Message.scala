package offGridOrcs

sealed trait Message extends MapMessage
object Message {
  final case class Animate(timeStep: Duration) extends Message
  final case class KeyDown(key: Key) extends Message
  final case class KeyUp(key: Key) extends Message
  final case class LeftClick(position: Vec2) extends Message
  final case class MouseMove(position: Vec2) extends Message
  final case class MouseLeave() extends Message
}

sealed trait MapMessage
object MapMessage {
  final case class StartScrollX(speed: Double) extends MapMessage
  final case class StopScrollX() extends MapMessage
  final case class StartScrollY(speed: Double) extends MapMessage
  final case class StopScrollY() extends MapMessage
  final case class ZoomIn() extends MapMessage
  final case class ZoomOut() extends MapMessage
  final case class Reset() extends Message
}
