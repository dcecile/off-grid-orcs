package offGridOrcs

sealed trait Precondition

object Precondition {
  final case class UnobstructedPath() extends Precondition
  final case class NoGoals() extends Precondition
  final case class CanCarryMore() extends Precondition
}
