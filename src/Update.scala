package offGridOrcs

object Update {
  def tileShade(tile: Tile): Double = {
    (tile.shade + 0.01) % 1
  }
}
