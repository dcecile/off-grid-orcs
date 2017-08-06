package offGridOrcs

final case class Time(frameNumber: Double) extends AnyVal {
  def +(duration: Duration): Time =
    Time(frameNumber + duration.totalFrames)

  def -(other: Time): Duration =
    Duration(frameNumber - other.frameNumber)

  def isReached(currentTime: Time): Boolean =
    (currentTime - this).totalFrames >= 0
}

object Time {
  val Zero = Time(0)
}
