package offGridOrcs

object View {
  def viewTileColor(tile: Tile): Vec3 = {
    val baseColor = tile.shade match {
      case Shade.None() =>
        Colors.Forest
      case Shade.Highlight() =>
        Colors.ForestHighlight
      case Shade.Shadow() =>
        Colors.ForestShadow
    }
    tile.orc match {
      case Some(_) =>
        Colors.Orc.mix(baseColor, Colors.ForestCover)
      case None =>
        baseColor
    }
  }

  def viewSprites(mapModel: Model.Map): Seq[Sprite] = {
    val cursor = mapModel.cursor
    cursor.position match {
      case Some(position) =>
        val spriteBuffer = cursor.action.spriteBuffer
        Seq(Sprite(
          position - Vec2.One * (spriteBuffer.size.toDouble / 2 - 1),
          spriteBuffer,
          1.0))
      case None =>
        Seq()
    }
  }
}
