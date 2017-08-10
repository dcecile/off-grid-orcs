package offGridOrcs

sealed trait Step {
  val completionTime: Time
}

object Step {
  final case class Walk(destination: Vec2, completionTime: Time) extends Step
  final case class ChopWood(goal: Reference.Goal, completionTime: Time) extends Step
}

object StepExtensions {
  implicit final class StepSeq(val seq: Seq[Step]) extends AnyVal {
    def followedBy(duration: Duration, partialStep: Time => Step): Seq[Step] = {
      val lastTime = seq.last.completionTime
      seq ++ Seq(partialStep(lastTime + duration))
    }
  }
}
