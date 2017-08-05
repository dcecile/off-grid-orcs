package offGridOrcs

sealed trait Message
object Message {
  final case class Animate() extends Message
  final case class StartScrollX(speed: Double) extends Message
  final case class StopScrollX() extends Message
  final case class StartScrollY(speed: Double) extends Message
  final case class StopScrollY() extends Message
  final case class ZoomIn() extends Message
  final case class ZoomOut() extends Message
}
