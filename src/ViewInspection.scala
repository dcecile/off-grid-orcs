package offGridOrcs

object ViewInspection {
  def view(model: Model.Inspection): Seq[Sprite] = {
    val tile = model.mapModel.world(model.topLeft)
    (viewBackground()
      ++ viewGrid()
      ++ viewTitle(tile)
      ++ viewCloseButton()
      ++ viewDetails(tile)
      ++ viewModeButton('A', 0, true)
      ++ viewModeButton('S', 1, false))
  }

  def viewBackground(): Seq[Sprite] = {
    Seq(Sprite(
      Vec2.Zero,
      BitmapLibrary.InspectScreen))
  }

  def viewGrid(): Seq[Sprite] = {
    Seq(Sprite(
      Vec2(4, 5),
      BitmapLibrary.InspectGrid(0)))
  }

  def viewTitle(tile: Tile): Seq[Sprite] = {
    val text = tile match {
      case Tile(_, _, Some(_), _) => "ORC"
      case Tile(_, Tile.Trees(_), _, _) => "TREES"
      case Tile(_, Tile.Grass(_), _, _) => "GRASS"
      case Tile(_, Tile.Building(_), _, _) => "HQ"
    }
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

  def viewDetails(tile: Tile): Seq[Sprite] = {
    val topLeft = Vec2(4, 18)
    val healthyAndGreen = Seq("HEALTHY", "GREEN")
    val lines = tile match {
      case Tile(_, _, Some(_), _) => healthyAndGreen
      case Tile(_, Tile.Trees(_), _, _) => healthyAndGreen
      case Tile(_, Tile.Grass(_), _, _) => healthyAndGreen
      case Tile(_, Tile.Building(_), _, _) => Seq("STURDY")
    }
    lines
      .zip(Stream.iterate(0)(_ + 1))
      .map({ case (line, index) => Glyph.getSprites(
        topLeft + Vec2(0, 8 * index.toDouble),
        line,
        _.boldBitmap)
      })
      .flatten
  }
}
