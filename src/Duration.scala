package offGridOrcs

final case class Duration(totalFrames: Double) extends AnyVal {
  def +(other: Duration) =
    Duration(totalFrames + other.totalFrames)

  def -(other: Duration) =
    Duration(totalFrames - other.totalFrames)

  def *(scale: Double) =
    Duration(totalFrames * scale)
}

object Duration {
  val Zero = Duration(0)

  def fromJSTimestamps(previousJSTimestamp: Double, currentJSTimestamp: Double): Duration = {
    val diffInMilliseconds = currentJSTimestamp - previousJSTimestamp
    val frameDurationInMilliseconds = (1.0 / 60) * 1000
    val totalFrames = diffInMilliseconds / frameDurationInMilliseconds
    val roundedToQuarterFrames = (totalFrames * 4 + 0.5).floor / 4
    val cappedFrames = roundedToQuarterFrames min 4
    Duration(cappedFrames)
  }
}
