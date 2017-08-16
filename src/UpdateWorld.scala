package offGridOrcs

object UpdateWorld {
  def update(isPaused: Boolean, world: World, message: MapMessage): World = {
    (isPaused, message) match {
      case (false, Message.Animate(duration)) =>
        animateWorld(world.copy(currentTime =
          world.currentTime + duration))
      case _ =>
        world
    }
  }

  def animateWorld(world: World): World = {
    world
      .foldLeft(_.buildings, findNextOrcs)
      .foldLeft(_.orcs, updateOrcPlan)
      .foldLeft(_.orcs, executeOrcPlan)
  }

  def updateOrcPlan(world: World, orc: Orc): Seq[Command] = {
    AI.reevaluatePlan(orc, world) match {
      case Some(newPlan) =>
        Seq(Command.UpdateOrc(
          orc.copy(plan = newPlan)))
      case None =>
        Seq()
    }
  }

  def findNextOrcs(world: World, building: Building): Seq[Command] = {
    building.nextOrcTime match {
      case Some(time) =>
        if (time.isReached(world.currentTime)) {
          Seq(
            Command.InsertOrc(Orc(
              _, building.entrancePosition, Plan.Zero, Stock.Zero)),
            Command.UpdateBuilding(building.copy(
              currentOrcs = building.currentOrcs + 1,
              nextOrcTime = if (building.currentOrcs + 1 < building.blueprint.housingCapacity) {
                Some(world.currentTime + Timings.OrcHousingSpeed)
              } else {
                None
              })))
        } else {
          Seq()
        }
      case None =>
        Seq()
    }
  }

  def executeOrcPlan(world: World, orc: Orc): Seq[Command] = {
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
        val newOrc = orc.copy(
          stock = orc.stock + Stock.Wood(1))
        (updateChoppedWoodShade(world(newOrc.position), world)
          ++ Seq(Command.UpdateOrc(newOrc)))
      case _: Step.DropStock =>
        val oldTile = world(orc.position)
        val newTile = oldTile.copy(
          stock = oldTile.stock + orc.stock)
        val newOrc = orc.copy(
          stock = Stock.Zero)
        Seq(
          Command.UpdateTile(newTile),
          Command.UpdateOrc(newOrc))
      case buildStep: Step.BuildFlooring =>
        executeOrcBuildStep(orc, world, buildStep, Tile.Flooring())
      case buildStep: Step.BuildWalls =>
        executeOrcBuildStep(orc, world, buildStep, Tile.Walls())
      case buildStep: Step.BuildRoof =>
        (executeOrcBuildStep(orc, world, buildStep, Tile.Roof())
          ++ updateBuildingShade(orc.position, world))
      case buildStep: Step.AddDecal =>
        (executeOrcBuildStep(orc, world, buildStep, Tile.Decal())
          ++ updateBuildingShade(orc.position, world))
    }
  }

  def executeOrcBuildStep(orc: Orc, world: World, buildStep: Step.Build, stage: Tile.BuildingStage): Seq[Command] = {
    val oldBuildTile = world(orc.position)
    val newBuildTile = oldBuildTile.copy(structure = Tile.Building(stage))
    val oldStockpileTile = world(buildStep.stockpilePosition)
    val newStockpileTile = oldStockpileTile.copy(
      stock = oldStockpileTile.stock - Stock.Wood(2))
    Seq(
      Seq(
        Command.UpdateTile(newBuildTile),
        Command.UpdateTile(newStockpileTile),
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
            Tile.Grass(Tile.SoftShadow()))
      })
      .map(Command.UpdateTile)
  }

  val highlightDirections = Seq(Vec2(-1, 0), Vec2(0, -1))

  val shadowDirections = Seq(Vec2(-1, 0), Vec2(0, -1), Vec2(-1, -1))

  def findNearbyTiles(position: Vec2, world: World, directions: Seq[Vec2]): Seq[Tile] = {
    directions
      .map(_ + position)
      .filter(world.isPositionValid(_))
      .map(world(_))
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

  def pickNewBlueprint(world: World): Blueprint = {
    val activeBlueprints = (world.activeGoals.map(_.blueprint)
      ++ world.buildings.map(_.blueprint))
    if (activeBlueprints.exists(_.isHeadquarters)) {
      BlueprintLibrary.Home
    } else {
      BlueprintLibrary.Headquarters
    }
  }
}
