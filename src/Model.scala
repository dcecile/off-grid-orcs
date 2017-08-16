package offGridOrcs

sealed trait Model

object Model {
  final case class Title() extends Model
  final case class Map(world: World, isPaused: Boolean, camera: Camera, cursor: Cursor.Map, previousInspectionMode: InspectionMode) extends Model
  final case class Inspection(topLeft: Vec2, selection: Vec2, mode: InspectionMode, cursor: Cursor.Inspection, mapModel: Model.Map) extends Model
  final case class Menu(mode: MenuMode, cursor: Cursor.Inspection, mapModel: Model.Map) extends Model

  sealed trait InspectionMode
  object InspectionMode {
    final case class Status() extends InspectionMode
    final case class Stock() extends InspectionMode
  }

  sealed trait MenuMode
  object MenuMode {
    final case class Normal() extends MenuMode
    final case class GameOver() extends MenuMode
  }
}
