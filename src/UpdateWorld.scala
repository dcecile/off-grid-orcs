package offGridOrcs

import scala.util.Random

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
    spawnDemons(world)
      .foldLeft(_.buildings, findNextOrcs)
      .foldLeft(_.activeOrcs, updateOrcPlan)
      .foldLeft(_.activeOrcs, executeOrcPlan)
      .foldLeft(_.activeDemons, moveDemon)
      .foldLeft(_.activeOrcs, attackDemons)
      .foldLeft(_.activeDemons, attackOrcs)
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
                Some(world.currentTime + Timings.OrcHousingSpeed + Timings.WalkSpeed * Random.nextDouble())
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

  def spawnDemons(world: World): World = {
    if (world.demonSpawnTime.isReached(world.currentTime)) {
      val orcsX = world.activeOrcs.map(_.position.x)
      val newDemons = spawnDemons(
        orcsX.min,
        orcsX.max,
        world.currentTime,
        Demon.currentSpawnNumber(world.demonWaveNumber))
      val commands = newDemons.map(Command.InsertDemon)
      world.execute(commands).copy(
        demonWaveNumber = world.demonWaveNumber + 1,
        demonSpawnTime = Demon.nextSpawnTime(
          world.demonWaveNumber + 1,
          world.currentTime))
    } else {
      world
    }
  }

  def spawnDemons(minX: Double, maxX: Double, currentTime: Time, count: Int): Seq[Reference.Demon => Demon] = {
    val diffX = (maxX - minX) max (count.toDouble * 2)
    val centerX = (minX + maxX) / 2
    def generateX() = {
      val offsetX = (Random.nextDouble() - 0.5) * diffX
      ((centerX + offsetX).floor max 0) min (Dimensions.MapSize - 1)
    }
    val startPositions = Stream.continually(
      Vec2(generateX(), 0))
    val destinationPositions = Stream.continually(
      Vec2(generateX(), Dimensions.MapSize - 1))
    val walkDurations = Stream.continually(Timings.DemonWalkSpeed).flatten
    val walkTimes = walkDurations.scanLeft(currentTime)(_ + _).drop(1)
    startPositions
      .zip(destinationPositions)
      .take(count)
      .map({ case (start, destination) => (id: Reference.Demon) =>
        val diff = destination - start
        val directionX = Seq.fill(diff.x.abs.toInt)(Vec2(diff.x.signum.toDouble, 0))
        val directionY = Seq.fill(diff.y.abs.toInt)(Vec2(0, diff.y.signum.toDouble))
        val directionSequence = Random.shuffle(directionX ++ directionY)
        val walkDestinations = directionSequence.scanLeft(start)(_ + _).drop(1)
        val steps = walkDestinations
          .zip(walkTimes)
          .map(Function.tupled(Step.Walk))
        Demon(id, start, steps, currentTime)
      })
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

  def moveDemon(world: World, demon: Demon): Seq[Command] = {
    demon.path match {
      case pathHead :: pathTail =>
        if (pathHead.completionTime.isReached(world.currentTime)) {
          Seq(Command.UpdateDemon(demon.copy(
            position = pathHead.destination,
            path = pathTail)))
        } else {
          Seq()
        }
      case Nil =>
        Seq(Command.DeleteDemon(demon))
    }
  }

  def attackDemons(world: World, orc: Orc): Seq[Command] = {
    val positions = for (y <- -1 to +1; x <- -1 to +1) yield orc.position + Vec2(x.toDouble, y.toDouble)
    val demons = positions
      .filter(world.isPositionValid(_))
      .map(world(_))
      .map(_.demon)
      .flatten
      .map(world(_))
      .filter(_ => Random.nextDouble() > 0.5)
    demons.map(Command.DeleteDemon)
  }

  def attackOrcs(world: World, demon: Demon): Seq[Command] = {
    val positions = for (y <- -1 to +1; x <- -1 to +1) yield demon.position + Vec2(x.toDouble, y.toDouble)
    val orcs = positions
      .filter(world.isPositionValid(_))
      .map(world(_))
      .map(_.orc)
      .flatten
      .map(world(_))
    orcs.map(Command.DeleteOrc)
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
