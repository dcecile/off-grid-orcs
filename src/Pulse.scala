package offGridOrcs

final case class Pulse(startTime: Time, period: Duration, start: Double, end: Double) {
  def apply(currentTime: Time): Double = {
    val input = ((currentTime - startTime).totalFrames / period.totalFrames) % 1
    val output = 1 - Math.pow(1 - input, 1)
    start + output * (end - start)
  }
}

object Pulse {
  val One = Pulse(Time.Zero, Duration(1), 1, 1)
}
