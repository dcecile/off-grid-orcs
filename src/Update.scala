package offGridOrcs

object Update {
  def updateModel(model: Model, message: Message): Model = {
    model.copy(uiState =
      updateUIState(model.uiState, message))
  }

  def updateUIState(uiState: UIState, message: Message): UIState = {
    uiState match {
      case _: UIState.Title =>
        message match {
          case Message.LeftClick(_) =>
            Initialize.initializeMapState()
          case _ =>
            uiState
        }
      case mapState: UIState.Map =>
        message match {
          case Message.Reset() =>
            Initialize.initializeTitleState()
          case _ =>
            mapState.copy(camera =
              updateCamera(mapState.camera, message))
        }
    }
  }

  def updateCamera(camera: Camera, message: Message): Camera = {
    message match {
      case Message.Animate() =>
        if (camera.velocity != Vec2.zero) {
          camera
            .copy(topLeft = (camera.topLeft + camera.velocity))
            .clamp()
        } else {
          camera
        }
      case Message.StartScrollX(speed) =>
        camera.copy(velocity =
          camera.velocity.copy(x = speed))
      case Message.StopScrollX() =>
        camera.copy(velocity =
          camera.velocity.copy(x = 0))
      case Message.StartScrollY(speed) =>
        camera.copy(velocity =
          camera.velocity.copy(y = speed))
      case Message.StopScrollY() =>
        camera.copy(velocity =
          camera.velocity.copy(y = 0))
      case Message.ZoomIn() =>
        camera.copy(zoomOut = ZoomOut.OneX())
      case Message.ZoomOut() =>
        camera.copy(zoomOut = ZoomOut.TwoX())
      case _ =>
        camera
    }
  }
}
