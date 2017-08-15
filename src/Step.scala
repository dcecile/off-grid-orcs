package offGridOrcs

sealed trait Step {
  val completionTime: Time
}

object Step {
  final case class Walk(destination: Vec2, completionTime: Time) extends Step
  final case class DropStock(completionTime: Time) extends Step
  final case class ChopWood(completionTime: Time) extends Step

  sealed trait Build extends Step {
    val stockpilePosition: Vec2
  }
  final case class BuildFlooring(stockpilePosition: Vec2, completionTime: Time) extends Build
  final case class BuildWalls(stockpilePosition: Vec2, completionTime: Time) extends Build
  final case class BuildRoof(stockpilePosition: Vec2, completionTime: Time) extends Build
  final case class AddDecal(stockpilePosition: Vec2, completionTime: Time) extends Build
}

object StepExtensions {
  implicit final class StepSeq(val seq: Seq[Step]) extends AnyVal {
    def followedBy(currentTime: Time, duration: Duration, partialStep: Time => Step): Seq[Step] = {
      val lastTime = seq.lastOption.map(_.completionTime).getOrElse(currentTime)
      seq ++ Seq(partialStep(lastTime + duration))
    }
  }
}
