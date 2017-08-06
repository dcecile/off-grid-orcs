package offGridOrcs

sealed trait Step

object Step {
  final case class Walk(direction: Vec2, arrivalTime: Time) extends Step
}
