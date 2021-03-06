package offGridOrcs

sealed trait Command

object Command {
  final case class InsertOrc(partialOrc: Reference.Orc => Orc) extends Command
  final case class UpdateOrc(newOrc: Orc) extends Command
  final case class DeleteOrc(oldOrc: Orc) extends Command
  final case class InsertDemon(partialDemon: Reference.Demon => Demon) extends Command
  final case class UpdateDemon(newOrc: Demon) extends Command
  final case class DeleteDemon(oldOrc: Demon) extends Command
  final case class UpdateBuilding(newBuilding: Building) extends Command
  final case class InsertGoal(partialGoal: Reference.Goal => Goal) extends Command
  final case class UpdateTile(newTile: Tile) extends Command
}
