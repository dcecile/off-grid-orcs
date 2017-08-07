package offGridOrcs

sealed trait Command

object Command {
  final case class InsertOrc(position: Vec2, plan: Plan) extends Command
  final case class UpdateOrc(newOrc: Orc) extends Command
}
