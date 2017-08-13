package offGridOrcs

object UpdateMap {
  def update(model: Model.Map, message: Message): Model = {
    message match {
      case Message.Reset() =>
        Initialize.initializeModel()
      case Message.LeftClick(position) =>
        handleLeftClick(model, position)
      case _ =>
        Model.Map(
          UpdateWorld.update(model.world, message),
          updateCamera(model.camera, message),
          updateCursor(model.cursor, message))
    }
  }

  def handleLeftClick(model: Model.Map, clickPosition: Vec2): Model = {
    val action = model.cursor.action
    val newPosition = action.clamp(clickPosition)
    val topLeft = model.camera.topLeft + newPosition.spriteTopLeft(action.bitmap.size)
    action match {
      case Cursor.Build() =>
        model.copy(
          world = UpdateWorld.startBlueprint(
            model.world,
            topLeft,
            BlueprintLibrary.Headquarters),
          cursor = model.cursor.copy(
            action = Cursor.Inspect(),
            position = Some(clickPosition)).clamp)
      case Cursor.Inspect() =>
        Model.Inspection(topLeft + Vec2(2, 2), model)
      case _ =>
        model
    }
  }

  def updateCamera(camera: Camera, message: Message): Camera = {
    message match {
      case Message.Animate(duration) =>
        if (camera.velocity != Vec2.Zero) {
          camera
            .copy(topLeft =
              (camera.topLeft + camera.velocity * camera.zoomOut.value * duration.totalFrames))
            .clamp
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

  def updateCursor(cursor: Cursor, message: Message): Cursor = {
    message match {
      case Message.MouseMove(position) =>
        cursor.copy(position = Some(position)).clamp
      case Message.MouseLeave() =>
        cursor.copy(position = None)
      case Message.ZoomIn() =>
        cursor.action match {
          case Cursor.ZoomedOut(zoomedInAction) =>
            cursor.copy(action = zoomedInAction).clamp
          case _ =>
            cursor
        }
      case Message.ZoomOut() =>
        cursor.action match {
          case Cursor.ZoomedOut(_) =>
            cursor
          case zoomedInAction =>
            cursor.copy(action = Cursor.ZoomedOut(zoomedInAction)).clamp
        }
      case _ =>
        cursor
    }
  }
}
