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
    }
    val withOrc = tile.orc match {
      case Some(_) =>
        Colors.Orc.mix(baseColor, tile.structure match {
          case _: Tile.Trees => Colors.ForestCover
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
