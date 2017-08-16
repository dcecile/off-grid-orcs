package offGridOrcs

object Timings {
  val IdleSequence = Seq(
    Duration(60),
    Duration(120),
    Duration(120),
    Duration(240))
  val WalkSpeed = Duration(5)
  val ChopWoodSpeed = Duration(11)
  val DropSpeed = Duration(7)
  val BuildSpeed = Duration(9)
  val DemonWalkSpeed = Seq(
    Duration(10),
    Duration(11))
  val DemonColorPulse = Duration(200)
  val DemonLightenPulse = Duration(25)
  val BlueprintPulse = Duration(90)
  val GoalPulse = Duration(300)
  val OrcHousingSpeed = Duration(300)
}
