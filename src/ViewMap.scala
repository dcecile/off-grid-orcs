package offGridOrcs

object ViewMap {
  def viewOverlay(model: Model.Map): Seq[Sprite] = {
    val currentAction = model.cursor.action match {
      case Cursor.ZoomedOut(zoomedInAction) =>
        zoomedInAction
      case zoomedInAction =>
        zoomedInAction
    }
    (viewPaused(model)
      ++ viewCursor(model)
      ++ viewModeButton('A', 0, currentAction == Cursor.Inspect())
      ++ viewModeButton('D', 2, currentAction == Cursor.Build())
      ++ viewModeButton('G', 4, model.isPaused))
  }

  def viewPaused(model: Model.Map): Seq[Sprite] = {
    model.isPaused match {
      case true => Seq(
        Sprite(
          Vec2.Zero,
          BitmapLibrary.MapPausedOverlay))
      case false => Seq()
    }
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

  def viewModeButton(char: Char, index: Int, isActive: Boolean): Seq[Sprite] = {
    val topLeft = Vec2(
      4 + 12 * index.toDouble, Dimensions.LowRez - 14)
    Seq(
      Sprite(
        topLeft,
        if (isActive) {
          BitmapLibrary.MapButton
        } else {
          BitmapLibrary.MapReverseButton
        }),
      Glyph.getSprite(
        topLeft + Vec2(2, 2),
        char,
        if (isActive) {
          _.faintBitmap
        } else {
          _.faintReverseBitmap
        }))
  }
}
