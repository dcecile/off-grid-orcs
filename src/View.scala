package offGridOrcs

object View {
  def tileColor(tile: Tile): Vec3 = {
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
}
