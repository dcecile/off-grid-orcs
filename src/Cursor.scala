package offGridOrcs

final case class Cursor(position: Option[Vec2], action: Cursor.Action) {
  def clamp = {
    position match {
      case Some(position) =>
        if (isTopOrLeftOutOfRange(position.x) || isTopOrLeftOutOfRange(position.y)) {
          this.copy(position = None)
        } else {
          this
        }
      case None =>
        this
    }
  }

  def isTopOrLeftOutOfRange(value: Double) = {
    val halfSize = action.spriteBuffer.size / 2
    (value.toInt < halfSize
      || value.toInt >= Dimensions.LowRez - halfSize)
  }
}

object Cursor {
  sealed trait Action {
    val spriteBuffer: SpriteBuffer
    val pulse = Pulse.One
  }

  final case class Inspect() extends Action {
    val spriteBuffer = Bitmaps.inspectCursor
  }

  final case class Build() extends Action {
    val spriteBuffer = Bitmaps.blueprintCursor
    override val pulse = Pulse(
      Time.Zero, Timings.BlueprintPulse, Colors.BlueprintStart, 1.0)
  }

  final case class ZoomedOut(zoomedInAction: Action) extends Action {
    val spriteBuffer = Bitmaps.zoomedOutCursor
  }
}
