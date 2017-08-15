package offGridOrcs

final case class Cursor[A <: Cursor.Action](position: Option[Vec2], action: A) {
  def clamp: Cursor[A] =
    this.copy(position = position.map(action.clamp))
}

object Cursor {
  type Map = Cursor[Cursor.Action]
  type Inspection = Cursor[Cursor.Overlay]

  sealed trait Action {
    val bitmap: Bitmap
    val pulse = Pulse.One
    def clamp(oldPosition: Vec2): Vec2 = {
      val size = bitmap.size
      val topLeft = oldPosition.spriteTopLeft(size)
      val newPosition = topLeft.clamp(
        Vec2.Zero,
        Vec2.One * (Dimensions.LowRez - size.toDouble))
      newPosition + (oldPosition - topLeft)
    }
  }

  final case class Inspect() extends Action {
    val bitmap = BitmapLibrary.InspectCursor
  }

  final case class Build(blueprint: Blueprint) extends Action {
    val bitmap = blueprint.cursorBitmap
    override val pulse = Pulse(
      Time.Zero, Timings.BlueprintPulse, Colors.BlueprintPulseStart, 1.0, Pulse.Linear())
  }

  final case class ZoomedOut(zoomedInAction: Action) extends Action {
    val bitmap = BitmapLibrary.ZoomedOutCursor
  }

  final case class Overlay() extends Action {
    val bitmap = BitmapLibrary.OverlayCursor
  }
}
