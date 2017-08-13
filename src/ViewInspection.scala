package offGridOrcs

object ViewInspection {
  def view(model: Model.Inspection): Seq[Sprite] = {
    val titleAndDescription = getTitleAndDescription(model)
    (viewBackground()
      ++ viewGrid(model)
      ++ viewTitle(titleAndDescription._1)
      ++ viewCloseButton()
      ++ viewDetails(titleAndDescription._2)
      ++ viewModeButton('A', 0, model.mode == Model.InspectionMode.Status())
      ++ viewModeButton('S', 1, model.mode == Model.InspectionMode.Stock())
      ++ viewCursor(model))
  }

  def getTitleAndDescription(model: Model.Inspection): (String, Seq[String]) = {
    val tile = model.mapModel.world(model.topLeft + model.selection)
    val healthyAndGreen = Seq("HEALTHY", "GREEN")
    val noStock = Seq("NO STOCK")
    tile match {
      case Tile(_, _, Some(_), _) =>
        ("ORC", model.mode match {
          case Model.InspectionMode.Status() =>
            healthyAndGreen
          case Model.InspectionMode.Stock() =>
            noStock
        })
      case Tile(_, Tile.Trees(_), _, _) =>
        ("TREES", model.mode match {
          case Model.InspectionMode.Status() =>
            healthyAndGreen
          case Model.InspectionMode.Stock() =>
            noStock
        })
      case Tile(_, Tile.Grass(_), _, _) =>
        ("GRASS", model.mode match {
          case Model.InspectionMode.Status() =>
            healthyAndGreen
          case Model.InspectionMode.Stock() =>
            noStock
        })
      case Tile(_, Tile.Building(_), _, _) =>
        ("HQ", model.mode match {
          case Model.InspectionMode.Status() =>
            Seq("STURDY")
          case Model.InspectionMode.Stock() =>
            noStock
        })
    }
  }

  def viewBackground(): Seq[Sprite] = {
    Seq(Sprite(
      Vec2.Zero,
      BitmapLibrary.InspectScreen))
  }

  def viewGrid(model: Model.Inspection): Seq[Sprite] = {
    val i = model.selection.x.toInt + model.selection.y.toInt * 3
    Seq(Sprite(
      Vec2(4, 5),
      BitmapLibrary.InspectGrid(i)))
  }

  def viewTitle(text: String): Seq[Sprite] = {
    Glyph.getSprites(Vec2(17, 4), text, _.boldLargeBitmap)
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

  def viewModeButton(char: Char, index: Int, isActive: Boolean): Seq[Sprite] = {
    val topLeft = Vec2(
      4 + 12 * index.toDouble, Dimensions.LowRez - 14)
    Seq(
      Sprite(
        topLeft,
        if (isActive) {
          BitmapLibrary.InspectButton
        } else {
          BitmapLibrary.InspectReverseButton
        }),
      Glyph.getSprite(
        topLeft + Vec2(2, 2),
        char,
        if (isActive) {
          _.boldBitmap
        } else {
          _.reverseBitmap
        }))
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

  def viewCursor(model: Model.Inspection): Seq[Sprite] = {
    ViewMap.viewCursor(model.cursor, Time.Zero)
  }
}
