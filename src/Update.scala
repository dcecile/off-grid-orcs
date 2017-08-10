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
      case Message.LeftClick(position) =>
        updateMapModelLeftClick(model, position)
      case _ =>
        Model.Map(
          updateWorld(model.world, message),
          updateCamera(model.camera, message),
          updateCursor(model.cursor, message))
    }
  }

  def updateMapModelLeftClick(model: Model.Map, clickPosition: Vec2): Model = {
    val action = model.cursor.action
    action match {
      case Cursor.Build() =>
        val newPosition = action.clamp(clickPosition)
        val topLeft = model.camera.topLeft + newPosition.spriteTopLeft(action.spriteBuffer.size)
        model.copy(
          world = model.world.execute(Command.InsertGoal(
            Blueprint.Headquarters.goal(topLeft, model.world.currentTime))),
          cursor = model.cursor.copy(
            action = Cursor.Inspect(),
            position = Some(clickPosition)).clamp)
      case _ =>
        model
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
    world.orcs.foldLeft(world)({ (world, orc) =>
      val commands = executeOrcPlan(
        AI.reevaluatePlan(orc, world),
        world)
      world.execute(commands)
    })
  }

  def executeOrcPlan(orc: Orc, world: World): Seq[Command] = {
    val step = orc.plan.head
    if (step.completionTime.isReached(world.currentTime)) {
      step match {
        case walk: Step.Walk =>
          Seq(
            Command.UpdateOrc(orc.copy(
              position = walk.destination,
              plan = orc.plan.tail)))
        case chopWood: Step.ChopWood =>
          val tile = world(orc.position)
          val goal = world(chopWood.goal)
          println(s"Chop ${orc.position}")
          Seq(
            Command.UpdateTile(tile.copy(
              structure = Tile.Grass())),
            Command.UpdateGoal(goal.copy(
              toClearPositions = goal.toClearPositions
                .diff(Seq(orc.position)))),
            Command.UpdateOrc(orc.copy(
              plan = orc.plan.tail)))
      }
    } else {
      Seq()
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
