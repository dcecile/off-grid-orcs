package offGridOrcs

sealed trait Command

object Command {
  final case class InsertOrc(partialOrc: Reference.Orc => Orc) extends Command
  final case class UpdateOrc(newOrc: Orc) extends Command
  final case class InsertGoal(partialGoal: Reference.Goal => Goal) extends Command
  final case class UpdateGoal(newGoal: Goal) extends Command
  final case class UpdateTile(newTile: Tile) extends Command
}
