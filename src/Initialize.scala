package offGridOrcs

object Initialize {
  def initializeModel(): Model = {
    Model.Title()
  }

  def initializeMapModel(cursorPosition: Vec2): Model.Map = {
    Model.Map(
      world = InitializeWorld.initialize(),
      isPaused = false,
      camera = initializeCamera(),
      cursor = initializeCursor(cursorPosition),
      previousInspectionMode = Model.InspectionMode.Status())
  }

  def initializeCamera(): Camera = {
    Camera(
      topLeft = Vec2.One
        * ((Dimensions.MapSize - Dimensions.LowRez) / 2).floor,
      velocity = Vec2.Zero,
      zoomOut = ZoomOut.OneX())
  }

  def initializeCursor(position: Vec2): Cursor.Map = {
    (Cursor(
      Some(position),
      Cursor.Build(BlueprintLibrary.Headquarters)): Cursor.Map).clamp
  }
}
