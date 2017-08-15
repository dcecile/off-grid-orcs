package offGridOrcs

object Timings {
  val IdleSequence = Seq(
    Duration(60),
    Duration(120),
    Duration(120),
    Duration(240))
  val WalkSpeed = Duration(5)
  val ChopWoodSpeed = Duration(10)
  val DropSpeed = Duration(10)
  val BuildSpeed = Duration(10)
  val BlueprintPulse = Duration(90)
  val GoalPulse = Duration(300)
}
