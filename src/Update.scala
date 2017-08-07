package offGridOrcs

object Update {
  def updateModel(model: Model, message: Message): Model = {
    model match {
      case titleModel: Model.Title =>
        updateTitleModel(titleModel, message)
      case mapModel: Model.Map =>
        updateMapModel(mapModel, message)
    }
  }

  def updateTitleModel(model: Model.Title, message: Message): Model = {
    message match {
      case Message.LeftClick(position) =>
        Initialize.initializeMapModel(position)
      case _ =>
        model
    }
  }

  def updateMapModel(model: Model.Map, message: Message): Model = {
    message match {
      case Message.Reset() =>
        Initialize.initializeModel()
      case _ =>
        Model.Map(
          updateWorld(model.world, message),
          updateCamera(model.camera, message),
          updateCursor(model.cursor, message))
    }
  }

  def updateWorld(world: World, message: Message): World = {
    message match {
      case Message.Animate(duration) =>
        animateWorld(world.copy(currentTime =
          world.currentTime + duration))
      case _ =>
        world
    }
  }

  def animateWorld(world: World): World = {
    val commands = animateOrc(
      world.orcs(0), world.currentTime)
    world.execute(commands)
  }

  def animateOrc(orc: Orc, currentTime: Time): Seq[Command] = {
    val newOrc = executeOrcPlan(orc, currentTime)
    Seq(Command.UpdateOrc(newOrc))
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
