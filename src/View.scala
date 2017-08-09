package offGridOrcs

object View {
  def viewTileColor(model: Model.Map, tile: Tile): Vec3 = {
    val baseColor = tile.shade match {
      case Shade.None() =>
        Colors.Forest
      case Shade.Highlight() =>
        Colors.ForestHighlight
      case Shade.Shadow() =>
        Colors.ForestShadow
    }
    val withOrc = tile.orc match {
      case Some(_) =>
        Colors.Orc.mix(baseColor, Colors.ForestCover)
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

  def viewSprites(model: Model.Map): Seq[Sprite] = {
    val cursor = model.cursor
    cursor.position match {
      case Some(position) =>
        val spriteBuffer = cursor.action.spriteBuffer
        Seq(Sprite(
          position.spriteTopLeft(spriteBuffer.size),
          spriteBuffer,
          cursor.action.pulse(model.world.currentTime)))
      case None =>
        Seq()
    }
  }
}
