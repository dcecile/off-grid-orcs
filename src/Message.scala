package offGridOrcs

sealed trait Message
object Message {
  final case class AnimationFrame(time: Double) extends Message
}

