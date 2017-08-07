package offGridOrcs

sealed trait Model

object Model {
  final case class Title() extends Model
  final case class Map(world: World, camera: Camera, cursor: Cursor) extends Model
}
