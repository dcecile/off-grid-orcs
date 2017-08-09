package offGridOrcs

final case class Pulse(startTime: Time, period: Duration, start: Double, end: Double, curve: Pulse.Curve) {
  def apply(currentTime: Time): Double = {
    val input = ((currentTime - startTime).totalFrames / period.totalFrames) % 1
    val output = curve(input)
    start + output * (end - start)
  }
}

object Pulse {
  sealed trait Curve {
    def apply(input: Double): Double
  }

  final case class Linear() extends Curve {
    def apply(input: Double): Double =
      input
  }

  final case class NegativeCosine() extends Curve {
    def apply(input: Double): Double =
      (-Math.cos(input * (2 * Math.PI)) + 1) / 2
  }

  val One = Pulse(Time.Zero, Duration(1), 1, 1, Linear())
}
