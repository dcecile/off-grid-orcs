package offGridOrcs

object Update {
  def updateModel(model: Model, message: Message): Model = {
    val newCurrentTime = updateCurrentTime(
      model.currentTime,
      model.uiState,
      message)
    val newUIState = updateUIState(
      model.uiState,
      message)
    val newOrc = Movement.handleOrcMovement(
      model.orc,
      updateOrc(
        model.orc,
        newCurrentTime,
        newUIState,
        message),
      model.tiles)
    Model(
      newCurrentTime,
      model.tiles,
      newOrc,
      newUIState)
  }

  def updateCurrentTime(currentTime: Time, uiState: UIState, message: Message): Time = {
    uiState match {
      case mapState: UIState.Map =>
        message match {
          case Message.Animate(duration) =>
            currentTime + duration
          case _ =>
            currentTime
        }
      case _ =>
        currentTime
    }
  }

  def updateOrc(orc: Orc, currentTime: Time, uiState: UIState, message: Message): Orc = {
    uiState match {
      case mapState: UIState.Map =>
        message match {
          case Message.Animate(_) =>
            executeOrcPlan(orc, currentTime)
          case _ => orc
        }
      case _ => orc
    }
  }

  def executeOrcPlan(orc: Orc, currentTime: Time): Orc = {
    orc.plan.head match {
      case Step.Walk(direction, arrivalTime) =>
        if (arrivalTime.isReached(currentTime)) {
          orc.copy(
            position = orc.position + direction,
            plan = refreshOrcPlan(orc.plan.tail, currentTime))
        } else {
          orc
        }
    }
  }

  def refreshOrcPlan(plan: Plan, currentTime: Time): Plan = {
    if (plan.steps.isEmpty) {
      Plan.idle(currentTime)
    } else {
      plan
    }
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
      case Message.Animate(duration) =>
        if (camera.velocity != Vec2.Zero) {
          camera
            .copy(topLeft =
              (camera.topLeft + camera.velocity * duration.totalFrames))
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
        camera.changeZoomOut(ZoomOut.OneX())
      case Message.ZoomOut() =>
        camera.changeZoomOut(ZoomOut.TwoX())
      case _ =>
        camera
    }
  }
}
