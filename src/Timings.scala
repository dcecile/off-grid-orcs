package offGridOrcs

object Timings {
  val IdleSequence = Seq(
    Duration(60),
    Duration(120),
    Duration(120),
    Duration(240))
  val WalkSpeed = Duration(10)
  val ChopWoodSpeed = Duration(40)
  val BlueprintPulse = Duration(90)
  val GoalPulse = Duration(120)
}
