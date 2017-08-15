package offGridOrcs

sealed trait Precondition

object Precondition {
  final case class NoGoals() extends Precondition
}
