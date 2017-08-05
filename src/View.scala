package offGridOrcs

object View {
  def tileColor(tile: Tile): Vec3 = {
    tile.shade match {
      case Shade.None() =>
        Colors.Forest
      case Shade.Highlight() =>
        Colors.ForestHighlight
      case Shade.Shadow() =>
        Colors.ForestShadow
    }
  }
}
