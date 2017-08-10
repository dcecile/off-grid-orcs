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
          val newShades = updateChoppedWoodShade(
            tile, world)
          val goal = world(chopWood.goal)
          val newGoal = goal.copy(
            toClearPositions = goal.toClearPositions
              .diff(Seq(orc.position)))
          val newOrc = orc.copy(
            plan = orc.plan.tail)
          Seq(
            newShades
              .map(Command.UpdateTile),
            Seq(
              Command.UpdateGoal(newGoal),
              Command.UpdateOrc(newOrc))).flatten
      }
    } else {
      Seq()
    }
  }

  def updateChoppedWoodShade(newGrass: Tile, world: World): Seq[Tile] = {
    val brightTrees = mapNearbyTiles(
      newGrass.position,
      world,
      highlightDirections,
      tile => tile.structure match {
        case Tile.Trees(_) =>
          Some(tile.copy(structure =
            Tile.Trees(Tile.HardHighlight())))
        case _ =>
          None
      }).flatten
    val normalGrass = mapNearbyTiles(
      newGrass.position,
      world,
      shadowDirections,
      tile => tile.structure match {
        case Tile.Grass(_) =>
          Some(tile.copy(structure =
            Tile.Grass(Initialize.initializeGrassShade(tile.position))))
        case _ =>
          None
      }).flatten.filter(tile => countShadows(tile.position, world) <= 1)
    val isShadowGrass = countShadows(newGrass.position, world) >= 1
    Seq(
      Seq(newGrass.copy(structure =
        Tile.Grass(if (isShadowGrass) {
          Tile.HardShadow()
        } else {
          Initialize.initializeGrassShade(newGrass.position)
        }))),
      brightTrees,
      normalGrass).flatten
  }

  val highlightDirections = Seq(Vec2(-1, 0), Vec2(0, -1))

  val shadowDirections = Seq(Vec2(-1, 0), Vec2(0, -1), Vec2(-1, -1))

  def mapNearbyTiles[A](position: Vec2, world: World, directions: Seq[Vec2], transform: Tile => A): Seq[A] = {
    directions
      .map(_ + position)
      .filter(isPositionValid)
      .map(world(_))
      .map(transform)
  }

  def isPositionValid(position: Vec2): Boolean = {
    position == position.clamp(Vec2.Zero, Vec2.One * (Dimensions.MapSize - 1))
  }

  def countShadows(position: Vec2, world: World): Int = {
    mapNearbyTiles(
      position,
      world,
      shadowDirections.map(_ * -1),
      tile => tile.structure match {
        case Tile.Trees(_) =>
          1
        case _ =>
          0
      }).sum
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
