package offGridOrcs

final case class Cursor(position: Option[Vec2], action: Cursor.Action) {
  def clamp =
    this.copy(position = position.map(action.clamp))
}

object Cursor {
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

  final case class Build() extends Action {
    val bitmap = BlueprintLibrary.Headquarters.cursorBitmap
    override val pulse = Pulse(
      Time.Zero, Timings.BlueprintPulse, Colors.BlueprintPulseStart, 1.0, Pulse.Linear())
  }

  final case class ZoomedOut(zoomedInAction: Action) extends Action {
    val bitmap = BitmapLibrary.ZoomedOutCursor
  }
}
