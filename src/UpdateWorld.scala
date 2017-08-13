package offGridOrcs

object UpdateWorld {
  def update(world: World, message: MapMessage): World = {
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
      executeOrcStep(
        orc.copy(plan = orc.plan.tail),
        world,
        step)
    } else {
      Seq()
    }
  }

  def executeOrcStep(orc: Orc, world: World, step: Step): Seq[Command] = {
    step match {
      case walk: Step.Walk =>
        Seq(Command.UpdateOrc(orc.copy(
          position = walk.destination)))
      case _: Step.ChopWood =>
        (updateChoppedWoodShade(world(orc.position), world)
          ++ Seq(Command.UpdateOrc(orc)))
      case _: Step.BuildFlooring =>
        executeOrcBuildStep(orc, world, Tile.Flooring())
      case _: Step.BuildWalls =>
        executeOrcBuildStep(orc, world, Tile.Walls())
      case _: Step.BuildRoof =>
        (executeOrcBuildStep(orc, world, Tile.Roof())
          ++ updateBuildingShade(orc.position, world))
      case _: Step.AddDecal =>
        (executeOrcBuildStep(orc, world, Tile.Decal())
          ++ updateBuildingShade(orc.position, world))
    }
  }

  def executeOrcBuildStep(orc: Orc, world: World, stage: Tile.BuildingStage): Seq[Command] = {
    val oldTile = world(orc.position)
    val newTile = oldTile.copy(structure = Tile.Building(stage))
    Seq(
      Seq(
        Command.UpdateTile(newTile),
        Command.UpdateOrc(orc))).flatten
  }

  def updateChoppedWoodShade(newGrass: Tile, world: World): Seq[Command.UpdateTile] = {
    val isShadowGrass = countShadows(newGrass.position, world) >= 1
    val newGrassShade = newGrass.copy(structure =
      Tile.Grass(if (isShadowGrass) {
        Tile.HardShadow()
      } else {
        InitializeWorld.initializeGrassShade(newGrass.position)
      }))
    val brightTrees = findNearbyTiles(newGrass.position, world, highlightDirections)
      .collect(tile => tile.structure match {
        case Tile.Trees(_) =>
          tile.copy(structure =
            Tile.Trees(Tile.HardHighlight()))
      })
    val normalGrass = findNearbyTiles(newGrass.position, world, shadowDirections)
      .collect(tile => tile.structure match {
        case Tile.Grass(_) =>
          tile.copy(structure =
            Tile.Grass(InitializeWorld.initializeGrassShade(tile.position)))
      })
      .filter(tile =>
        countShadows(tile.position, world) <= 1)
    Seq(Seq(newGrassShade), brightTrees, normalGrass)
      .flatten
      .map(Command.UpdateTile)
  }

  def updateBuildingShade(buildingPosition: Vec2, world: World): Seq[Command.UpdateTile] = {
    findNearbyTiles(buildingPosition, world, shadowDirections)
      .collect(tile => tile.structure match {
        case Tile.Grass(_) =>
          tile.copy(structure =
            Tile.Grass(Tile.HardShadow()))
      })
      .map(Command.UpdateTile)
  }

  val highlightDirections = Seq(Vec2(-1, 0), Vec2(0, -1))

  val shadowDirections = Seq(Vec2(-1, 0), Vec2(0, -1), Vec2(-1, -1))

  def findNearbyTiles(position: Vec2, world: World, directions: Seq[Vec2]): Seq[Tile] = {
    directions
      .map(_ + position)
      .filter(isPositionValid)
      .map(world(_))
  }

  def isPositionValid(position: Vec2): Boolean = {
    position == position.clamp(Vec2.Zero, Vec2.One * (Dimensions.MapSize - 1))
  }

  def countShadows(position: Vec2, world: World): Int = {
    findNearbyTiles(position, world, shadowDirections.map(_ * -1))
      .collect(tile => tile.structure match {
        case Tile.Trees(_) => ()
        case Tile.Building(Tile.Roof()) => ()
        case Tile.Building(Tile.Decal()) => ()
      })
      .length
  }

  def startBlueprint(world: World, position: Vec2, blueprint: Blueprint): World = {
    world.execute(Seq(Command.InsertGoal(
      Goal.fromBlueprint(
        _,
        position,
        blueprint,
        world.currentTime,
        world))))
  }
}
