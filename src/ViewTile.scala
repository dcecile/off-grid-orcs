package offGridOrcs

object ViewTile {
  def view(world: World, tile: Tile): Vec3 = {
    val baseColor = viewBaseColor(tile)
    val withOrc = tile.orc match {
      case Some(_) => Colors.Orc.mix(baseColor, viewOrcCover(tile))
      case None => baseColor
    }
    withOrc + (tile.goal match {
      case Some(id) => viewGoal(world, id)
      case None => Vec3.Zero
    })
  }

  def viewBaseColor(tile: Tile): Vec3 = {
    tile.structure match {
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
        if (tile.stock.wood > 0) {
          Colors.WoodPile
        } else {
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
  }

  def viewOrcCover(tile: Tile): Double = {
    tile.structure match {
      case Tile.Trees(_) =>
        Colors.ForestCover
      case Tile.Building(Tile.Walls()) =>
        Colors.BuildingWallsCover
      case Tile.Building(Tile.Roof()) =>
        Colors.BuildingRoofCover
      case Tile.Building(Tile.Decal()) =>
        Colors.BuildingRoofCover
      case _ => 0
    }
  }

  def viewGoal(world: World, id: Reference.Goal): Vec3 = {
    val goal = world(id)
    goal.color * goal.pulse(world.currentTime)
  }
}
