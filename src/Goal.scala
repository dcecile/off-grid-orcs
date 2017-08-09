package offGridOrcs

final case class Goal(id: Reference.Goal, allPositions: Seq[Vec2], toClearPositions: Seq[Vec2], toBuildFloorPositions: Seq[Vec2], toBuildWallPositions: Seq[Vec2], toBuildRoofPositions: Seq[Vec2], stockpilePositions: Seq[Vec2], startTime: Time) {
  val color = Colors.Goal
  val pulse = Pulse(
    Time.Zero, Timings.GoalPulse, Colors.GoalPulseStart, 1.0, Pulse.NegativeCosine())
}
