package offGridOrcs

object View {
  def viewTileColor(model: Model.Map, tile: Tile): Vec3 = {
    val baseColor = tile.structure match {
      case Tile.Trees(shade) =>
        shade match {
          case Tile.NoShade() =>
            Colors.Forest
          case Tile.HardHighlight() =>
            Colors.ForestHardHighlight
          case Tile.SoftHighlight() =>
            Colors.ForestSoftHighlight
          case Tile.SoftShadow() =>
            Colors.ForestSoftShadow
        }
      case Tile.Grass(shade) =>
        shade match {
          case Tile.NoShade() =>
            Colors.Grass
          case Tile.SoftHighlight() =>
            Colors.GrassSoftHighlight
          case Tile.SoftShadow() =>
            Colors.GrassSoftShadow
          case Tile.HardShadow() =>
            Colors.GrassHardShadow
        }
      case Tile.Building(stage) =>
        stage match {
          case Tile.Flooring() =>
            Colors.BuildingFlooring
          case Tile.Walls() =>
            Colors.BuildingWalls
          case Tile.Roof() =>
            Colors.BuildingRoof
          case Tile.Decal() =>
            Colors.BuildingDecal
        }
    }
    val withOrc = tile.orc match {
      case Some(_) =>
        Colors.Orc.mix(baseColor, tile.structure match {
          case Tile.Trees(_) =>
            Colors.ForestCover
          case Tile.Building(Tile.Walls()) =>
            Colors.BuildingWallsCover
          case Tile.Building(Tile.Roof()) =>
            Colors.BuildingRoofCover
          case Tile.Building(Tile.Decal()) =>
            Colors.BuildingRoofCover
          case _ => 0
        })
      case None =>
        baseColor
    }
    tile.goal match {
      case Some(goalID) =>
        val goal = model.world(goalID)
        withOrc + goal.color * goal.pulse(model.world.currentTime)
      case None =>
        withOrc
    }
  }

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

  def viewInspectionScreen(model: Model.Inspection): Seq[Sprite] = {
    val tile = model.mapModel.world(model.topLeft)
    (viewInspectionScreenBackground()
      ++ viewInspectionScreenGrid()
      ++ viewInspectionScreenTitle(tile)
      ++ viewInspectionScreenCloseButton()
      ++ viewInspectionScreenDetails(tile)
      ++ viewInspectionScreenModeButton('A', 0, true)
      ++ viewInspectionScreenModeButton('S', 1, false))
  }

  def viewInspectionScreenBackground(): Seq[Sprite] = {
    Seq(Sprite(
      Vec2.Zero,
      BitmapLibrary.InspectScreen))
  }

  def viewInspectionScreenGrid(): Seq[Sprite] = {
    Seq(Sprite(
      Vec2(4, 5),
      BitmapLibrary.InspectGrid(0)))
  }

  def viewInspectionScreenTitle(tile: Tile): Seq[Sprite] = {
    val text = tile match {
      case Tile(_, _, Some(_), _) => "ORC"
      case Tile(_, Tile.Trees(_), _, _) => "TREES"
      case Tile(_, Tile.Grass(_), _, _) => "GRASS"
      case Tile(_, Tile.Building(_), _, _) => "HQ"
    }
    getGlyphSprites(Vec2(17, 4), text, _.boldLargeBitmap)
  }

  def viewInspectionScreenCloseButton(): Seq[Sprite] = {
    val topLeft = Vec2(
      Dimensions.LowRez - 10, 1)
    Seq(
      Sprite(
        topLeft,
        BitmapLibrary.InspectCorner),
      getGlyphSprite(
        topLeft + Vec2(2, 1),
        'Q',
        _.boldBitmap))
  }

  def viewInspectionScreenModeButton(char: Char, index: Int, isActive: Boolean): Seq[Sprite] = {
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
      getGlyphSprite(
        topLeft + Vec2(2, 2),
        char,
        if (isActive) {
          _.boldBitmap
        } else {
          _.reverseBitmap
        }))
  }

  def viewInspectionScreenDetails(tile: Tile): Seq[Sprite] = {
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
      .map({ case (line, index) => getGlyphSprites(
        topLeft + Vec2(0, 8 * index.toDouble),
        line,
        _.boldBitmap)
      })
      .flatten
  }

  def getGlyphSprites(topLeft: Vec2, string: String, bitmap: Glyph => Bitmap): Seq[Sprite] = {
    val positions = (Stream.iterate(topLeft)
      (_ + Vec2(Glyph.size.toDouble + 1, 0)))
    string
      .zip(positions)
      .map({ case (char, position) =>
        getGlyphSprite(position, char, bitmap)
      })
  }

  def getGlyphSprite(topLeft: Vec2, char: Char, bitmap: Glyph => Bitmap): Sprite = {
    Sprite(topLeft, bitmap(getGlyph(char)))
  }

  def getGlyph(char: Char): Glyph = {
    GlyphLibrary.glyphs
      .filter(_.char == char)
      .headOption
      .getOrElse(GlyphLibrary.unknownGlyph)
  }
}
