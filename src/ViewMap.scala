package offGridOrcs

object ViewMap {
  def viewOverlay(model: Model.Map): Seq[Sprite] = {
    viewCursor(model)
  }

  def viewCursor(model: Model.Map): Seq[Sprite] = {
    viewCursor(model.cursor, model.world.currentTime)
  }

  def viewCursor[A <: Cursor.Action](cursor: Cursor[A], currentTime: Time): Seq[Sprite] = {
    cursor.position match {
      case Some(position) =>
        val bitmap = cursor.action.bitmap
        Seq(Sprite(
          position.spriteTopLeft(bitmap.size),
          bitmap,
          cursor.action.pulse(currentTime)))
      case None =>
        Seq()
    }
  }
}
