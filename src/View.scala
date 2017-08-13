package offGridOrcs

object View {
  def viewTileColor(world: World, tile: Tile): Vec3 =
    ViewTile.view(world, tile)

  def viewHeadsUpDisplay(model: Model.Map): Seq[Sprite] =
    ViewMap.viewHeadsUpDisplay(model)

  def viewInspectionScreen(model: Model.Inspection): Seq[Sprite] =
    ViewInspection.view(model)
}
