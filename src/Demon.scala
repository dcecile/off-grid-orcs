package offGridOrcs

final case class Demon(
  id: Reference.Demon,
  position: Vec2,
  path: Seq[Step.Walk],
  spawnTime: Time
) {
  val colorPulse = Pulse(
    spawnTime, Timings.DemonColorPulse, 0, 1, Pulse.Sine())
  val lightenPulse = Pulse(
    spawnTime, Timings.DemonLightenPulse, 0, Colors.DemonLighten, Pulse.Sine())
}

object Demon {
  def nextSpawnTime(waveNumber: Int, currentTime: Time): Time = {
    val delay = 4000 - waveNumber.toDouble * 400
    currentTime + Duration(delay max 800)
  }

  def currentSpawnNumber(waveNumber: Int): Int = {
    (waveNumber * waveNumber) min 64
  }
}
