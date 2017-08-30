package offGridOrcs

object ViewInspection {
  def view(model: Model.Inspection): Seq[Sprite] = {
    val titleAndDescription = getTitleAndDetails(model)
    (viewBackground()
      ++ viewGrid(model)
      ++ viewTitle(titleAndDescription._1)
      ++ viewCloseButton()
      ++ viewDetails(titleAndDescription._2)
      ++ viewModeButton('A', 0, model.mode == Model.InspectionMode.Status())
      ++ viewModeButton('S', 1, model.mode == Model.InspectionMode.Stock())
      ++ viewCursor(model))
  }

  def getTitleAndDetails(model: Model.Inspection): (String, Seq[String]) = {
    val world = model.mapModel.world
    val tile = world(model.topLeft + model.selection)
    val healthyAndGreen = Seq("HEALTHY", "GREEN")
    val fullData = tile match {
      case Tile(_, _, _, Some(_), _, _, _) =>
        ("DEMON",
          Seq("FIERY", "ANGRY"),
          getStockDetails(Stock.Zero))
      case Tile(_, _, Some(orcID), _, _, _, _) =>
        val orc = world(orcID)
        ("ORC",
          healthyAndGreen,
          getStockDetails(orc.stock))
      case Tile(_, _, _, _, Some(buildingID), _, _) =>
        val building = world(buildingID)
        (building.blueprint.name,
          Seq(
            "STURDY",
            s"${building.currentOrcs}/${building.blueprint.housingCapacity} ORCS"),
          getStockDetails(building.stock))
      case Tile(_, Tile.Trees(_), _, _, _, _, _) =>
        ("TREES",
          healthyAndGreen,
          getStockDetails(tile.stock))
      case Tile(_, Tile.Grass(_), _, _, _, _, _) =>
        ("GRASS",
          healthyAndGreen,
          getStockDetails(tile.stock))
      case Tile(_, Tile.Building(_), _, _, _, _, _) =>
        ("???",
          Seq("UNDER", "CNSTRCTN"),
          getStockDetails(tile.stock))
    }
    (fullData._1, model.mode match {
      case Model.InspectionMode.Status() =>
        fullData._2
      case Model.InspectionMode.Stock() =>
        fullData._3
    })
  }

  def getStockDetails(stock: Stock): Seq[String] = {
    if (stock.wood == 0) {
      Seq("NO STOCK")
    } else {
      Seq(s"${stock.wood} WOOD")
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
    GlyphBitmap.getSprites(Vec2(17, 4), text, _.boldLargeBitmap)
  }

  def viewCloseButton(): Seq[Sprite] = {
    val topLeft = Vec2(
      Dimensions.LowRez - 10, 1)
    Seq(
      Sprite(
        topLeft,
        BitmapLibrary.InspectCorner),
      GlyphBitmap.getSprite(
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
      GlyphBitmap.getSprite(
        topLeft + Vec2(2, 2),
        char,
        if (isActive) {
          _.boldBitmap
        } else {
          _.boldReverseBitmap
        }))
  }

  def viewDetails(lines: Seq[String]): Seq[Sprite] = {
    val topLeft = Vec2(4, 18)
    lines
      .zip(Stream.iterate(0)(_ + 1))
      .map({ case (line, index) => GlyphBitmap.getSprites(
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
