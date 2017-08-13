package offGridOrcs

object ViewMap {

  def viewHeadsUpDisplay(model: Model.Map): Seq[Sprite] = {
    val cursor = model.cursor
    val cursorSprite = cursor.position match {
      case Some(position) =>
        val bitmap = cursor.action.bitmap
        Seq(Sprite(
          position.spriteTopLeft(bitmap.size),
          bitmap,
          cursor.action.pulse(model.world.currentTime)))
      case None =>
        Seq()
    }
    cursorSprite
  }
}
