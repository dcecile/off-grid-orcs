package offGridOrcs

object UpdateMap {
  def update(model: Model.Map, message: Message): Model = {
    translate(model, message)
  }

  def update(model: Model.Map, message: MapMessage): Model = {
    message match {
      case MapMessage.Reset() =>
        Initialize.initializeModel()
      case Message.LeftClick(_) =>
        handleLeftClick(model)
      case _ =>
        model.copy(
          world = UpdateWorld.update(model.world, message),
          camera = updateCamera(model.camera, message),
          cursor = updateCursor(model.cursor, message))
    }
  }

  def translate(model: Model.Map, message: Message): Model = {
    val newModel = message match {
      case Message.MouseMove(position) =>
        moveMouse(model, Some(position))
      case Message.LeftClick(position) =>
        moveMouse(model, Some(position))
      case Message.MouseLeave() =>
        moveMouse(model, None)
      case _ => model
    }
    val newMessage = message match {
      case Message.KeyDown(key) =>
        translateKeyDown(key).getOrElse(message)
      case Message.KeyUp(key) =>
        translateKeyUp(key).getOrElse(message)
      case _ => message
    }
    update(newModel, newMessage)
  }

  def moveMouse(model: Model.Map, position: Option[Vec2]): Model.Map = {
    model.copy(
      cursor = model.cursor.copy(
        position = position).clamp)
  }

  def translateKeyDown(key: Key): Option[MapMessage] = {
    val scrollSpeed = 1.0
    key match {
      case Shortcuts.Map.ScrollLeft => Some(
        MapMessage.StartScrollX(-scrollSpeed))
      case Shortcuts.Map.ScrollRight => Some(
        MapMessage.StartScrollX(+scrollSpeed))
      case Shortcuts.Map.ScrollUp => Some(
        MapMessage.StartScrollY(-scrollSpeed))
      case Shortcuts.Map.ScrollDown => Some(
        MapMessage.StartScrollY(+scrollSpeed))
      case Shortcuts.Map.ZoomIn => Some(
        MapMessage.ZoomIn())
      case Shortcuts.Map.ZoomOut => Some(
        MapMessage.ZoomOut())
      case Shortcuts.Map.Reset => Some(
        MapMessage.Reset())
      case _ => None
    }
  }

  def translateKeyUp(key: Key): Option[MapMessage] = {
    key match {
      case Shortcuts.Map.ScrollLeft => Some(
        MapMessage.StopScrollX())
      case Shortcuts.Map.ScrollRight => Some(
        MapMessage.StopScrollX())
      case Shortcuts.Map.ScrollUp => Some(
        MapMessage.StopScrollY())
      case Shortcuts.Map.ScrollDown => Some(
        MapMessage.StopScrollY())
      case _ => None
    }
  }

  def handleLeftClick(model: Model.Map): Model = {
    val action = model.cursor.action
    val newPosition = model.cursor.position.get
    val topLeft = model.camera.topLeft + newPosition.spriteTopLeft(action.bitmap.size)
    action match {
      case Cursor.Build() =>
        model.copy(
          world = UpdateWorld.startBlueprint(
            model.world,
            topLeft,
            BlueprintLibrary.Headquarters),
          cursor = (model.cursor.copy(
            action = Cursor.Inspect(),
            position = Some(newPosition)): Cursor.Map).clamp)
      case Cursor.Inspect() =>
        Model.Inspection(
          topLeft + Vec2(2, 2),
          Vec2(1, 1),
          model.previousInspectionMode,
          Cursor(
            Some(newPosition),
            Cursor.Overlay()),
          model)
      case _ =>
        model
    }
  }

  def updateCamera(camera: Camera, message: MapMessage): Camera = {
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
      case MapMessage.StartScrollX(speed) =>
        camera.copy(velocity =
          camera.velocity.copy(x = speed))
      case MapMessage.StopScrollX() =>
        camera.copy(velocity =
          camera.velocity.copy(x = 0))
      case MapMessage.StartScrollY(speed) =>
        camera.copy(velocity =
          camera.velocity.copy(y = speed))
      case MapMessage.StopScrollY() =>
        camera.copy(velocity =
          camera.velocity.copy(y = 0))
      case MapMessage.ZoomIn() =>
        camera.changeZoomOut(ZoomOut.OneX())
      case MapMessage.ZoomOut() =>
        camera.changeZoomOut(ZoomOut.TwoX())
      case _ =>
        camera
    }
  }

  def updateCursor(cursor: Cursor.Map, message: MapMessage): Cursor.Map = {
    message match {
      case MapMessage.ZoomIn() =>
        cursor.action match {
          case Cursor.ZoomedOut(zoomedInAction) =>
            cursor.copy(action = zoomedInAction).clamp
          case _ =>
            cursor
        }
      case MapMessage.ZoomOut() =>
        cursor.action match {
          case Cursor.ZoomedOut(_) =>
            cursor
          case zoomedInAction =>
            (cursor.copy(action = Cursor.ZoomedOut(zoomedInAction)): Cursor.Map).clamp
        }
      case _ =>
        cursor
    }
  }
}
