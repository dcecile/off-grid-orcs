package offGridOrcs

object ViewMenu {
  def view(model: Model.Menu): Seq[Sprite] = {
    val titleAndDescription = getTitleAndDetails(model)
    (viewBackground()
      ++ viewTitle(titleAndDescription._1)
      ++ viewCloseButton()
      ++ viewDetails(titleAndDescription._2)
      ++ viewCursor(model))
  }

  def getTitleAndDetails(model: Model.Menu): (String, Seq[String]) = {
    val totalOrcs = model.mapModel.world.activeOrcs.length
    val totalHours = (model.mapModel.world.currentTime.frameNumber / 600).toInt
    model.mode match {
      case Model.MenuMode.Normal() =>
        ("STATUS",
          Seq(
            s"${totalOrcs} ORCS",
            s"${totalHours} HOURS"))
      case Model.MenuMode.GameOver() =>
        ("THE END",
          Seq(
            s"${totalOrcs} ORCS",
            s"${totalHours} HOURS"))
    }
  }

  def viewBackground(): Seq[Sprite] = {
    Seq(Sprite(
      Vec2.Zero,
      BitmapLibrary.InspectScreen))
  }

  def viewTitle(text: String): Seq[Sprite] = {
    Glyph.getSprites(Vec2(4, 4), text, _.boldLargeBitmap)
  }

  def viewCloseButton(): Seq[Sprite] = {
    val topLeft = Vec2(
      Dimensions.LowRez - 10, 1)
    Seq(
      Sprite(
        topLeft,
        BitmapLibrary.InspectCorner),
      Glyph.getSprite(
        topLeft + Vec2(2, 1),
        'Q',
        _.boldBitmap))
  }

  def viewDetails(lines: Seq[String]): Seq[Sprite] = {
    val topLeft = Vec2(4, 18)
    lines
      .zip(Stream.iterate(0)(_ + 1))
      .map({ case (line, index) => Glyph.getSprites(
        topLeft + Vec2(0, 8 * index.toDouble),
        line,
        _.boldBitmap)
      })
      .flatten
  }

  def viewCursor(model: Model.Menu): Seq[Sprite] = {
    ViewMap.viewCursor(model.cursor, Time.Zero)
  }
}
