package offGridOrcs

sealed trait Step {
  val completionTime: Time
}

object Step {
  final case class Walk(destination: Vec2, completionTime: Time) extends Step
  final case class DropStock(completionTime: Time) extends Step
  final case class ChopWood(completionTime: Time) extends Step
  final case class BuildFlooring(completionTime: Time) extends Step
  final case class BuildWalls(completionTime: Time) extends Step
  final case class BuildRoof(completionTime: Time) extends Step
  final case class AddDecal(completionTime: Time) extends Step
}

object StepExtensions {
  implicit final class StepSeq(val seq: Seq[Step]) extends AnyVal {
    def followedBy(duration: Duration, partialStep: Time => Step): Seq[Step] = {
      val lastTime = seq.last.completionTime
      seq ++ Seq(partialStep(lastTime + duration))
    }
  }
}
