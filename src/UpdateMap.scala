package offGridOrcs

object UpdateMap {
  def update(model: Model.Map, message: Message): Model = {
    translate(message) match {
      case MapMessage.Reset() =>
        Initialize.initializeModel()
      case Message.LeftClick(position) =>
        handleLeftClick(model, position)
      case otherMessage =>
        Model.Map(
          UpdateWorld.update(model.world, otherMessage),
          updateCamera(model.camera, otherMessage),
          updateCursor(model.cursor, otherMessage))
    }
  }

  def translate(message: Message): MapMessage = {
    message match {
      case Message.KeyDown(key) =>
        translateKeyDown(key).getOrElse(message)
      case Message.KeyUp(key) =>
        translateKeyUp(key).getOrElse(message)
      case _ => message
    }
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

  def updateCursor(cursor: Cursor, message: MapMessage): Cursor = {
    message match {
      case Message.MouseMove(position) =>
        cursor.copy(position = Some(position)).clamp
      case Message.MouseLeave() =>
        cursor.copy(position = None)
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
            cursor.copy(action = Cursor.ZoomedOut(zoomedInAction)).clamp
        }
      case _ =>
        cursor
    }
  }
}
