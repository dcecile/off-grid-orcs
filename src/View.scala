package offGridOrcs

object View {
  def viewTileColor(world: World, tile: Tile): Vec3 =
    ViewTile.view(world, tile)

  def viewMapOverlay(model: Model.Map): Seq[Sprite] =
    ViewMap.viewOverlay(model)

  def viewInspectionScreen(model: Model.Inspection): Seq[Sprite] =
    ViewInspection.view(model)

  def viewMenuScreen(model: Model.Menu): Seq[Sprite] =
    ViewMenu.view(model)
}
