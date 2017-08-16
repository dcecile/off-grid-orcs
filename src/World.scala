package offGridOrcs

import scala.scalajs.js

final case class World(
  currentTime: Time,
  tiles: js.Array[Tile],
  orcs: js.Array[Option[Orc]],
  demons: js.Array[Option[Demon]],
  buildings: js.Array[Building],
  goals: js.Array[Option[Goal]],
  demonWaveNumber: Int,
  demonSpawnTime: Time
) {
  def apply(position: Vec2): Tile = {
    val index = computeTileIndex(position)
    tiles(index)
  }

  def apply(id: Reference.Orc): Orc = {
    // Assume that all old references have been purged
    orcs(id.index).get
  }

  def apply(id: Reference.Demon): Demon = {
    // Assume that all old references have been purged
    demons(id.index).get
  }

  def apply(id: Reference.Building): Building = {
    buildings(id.index)
  }

  def apply(id: Reference.Goal): Goal = {
    // Assume that all old references have been purged
    goals(id.index).get
  }

  def isPositionValid(position: Vec2): Boolean = {
    position == position.clamp(Vec2.Zero, Vec2.One * (Dimensions.MapSize - 1))
  }

  def activeOrcs: Seq[Orc] = orcs.flatten
  def activeDemons: Seq[Demon] = demons.flatten
  def activeGoals: Seq[Goal] = goals.flatten

  def foldLeft[A](select: World => Seq[A], transform: (World, A) => Seq[Command]): World = {
    select(this).foldLeft(this)({ (world, item) =>
      val commands = transform(world, item)
      world.execute(commands)
    })
  }

  def execute(commands: Seq[Command]): World = {
    if (commands.nonEmpty) {
      commands.foldLeft(this)(_.execute(_))
        .updateGoals()
    } else {
      this
    }
  }

  private def execute(command: Command): World = {
    command match {
      case Command.InsertOrc(partialOrc) =>
        val id = Reference.Orc(orcs.length)
        val newOrc = partialOrc(id)
        setOrc(newOrc)
        setTileOrc(newOrc.position, Some(newOrc))
        this
      case Command.UpdateOrc(newOrc) =>
        val oldOrc = apply(newOrc.id)
        setOrc(newOrc)
        if (newOrc.position != oldOrc.position) {
          setTileOrc(oldOrc.position, None)
          setTileOrc(newOrc.position, Some(newOrc))
        }
        this
      case Command.DeleteOrc(oldOrc) =>
        unsetOrc(oldOrc.id)
        setTileOrc(oldOrc.position, None)
        this
      case Command.InsertDemon(partialDemon) =>
        val id = Reference.Demon(demons.length)
        val newDemon = partialDemon(id)
        setDemon(newDemon)
        setTileDemon(newDemon.position, Some(newDemon))
        this
      case Command.UpdateDemon(newDemon) =>
        val oldDemon = apply(newDemon.id)
        setDemon(newDemon)
        if (newDemon.position != oldDemon.position) {
          setTileDemon(oldDemon.position, None)
          setTileDemon(newDemon.position, Some(newDemon))
        }
        this
      case Command.DeleteDemon(oldDemon) =>
        unsetDemon(oldDemon.id)
        setTileDemon(oldDemon.position, None)
        this
      case Command.UpdateBuilding(newBuilding) =>
        setBuilding(newBuilding)
        this
      case Command.InsertGoal(partialGoal) =>
        val id = Reference.Goal(goals.length)
        val newGoal = partialGoal(id)
        setGoal(newGoal)
        for (position <- newGoal.allPositions) {
          setTileGoal(position, Some(newGoal))
        }
        this
      case Command.UpdateTile(newTile) =>
        setTile(newTile)
        this
    }
  }

  private def updateGoals(): World = {
    for (oldGoal <- activeGoals) {
      val newGoalOption = Some(oldGoal.fromBlueprint(this))
        .filter(_.isActive)
      newGoalOption match {
        case Some(newGoal) =>
          setGoal(newGoal)
        case None =>
          unsetGoal(oldGoal.id)
          for (position <- oldGoal.allPositions) {
            setTileGoal(position, None)
          }
          val newBuildingID = Reference.Building(buildings.length)
          val newBuilding = Building.fromBlueprint(newBuildingID, oldGoal.topLeft, oldGoal.blueprint, this)
          setBuilding(newBuilding)
          for (position <- newBuilding.positions) {
            setTileBuilding(position, Some(newBuilding))
          }
      }
    }
    this
  }

  private def setOrc(orc: Orc): Unit = {
    orcs.update(orc.id.index, Some(orc))
  }

  private def unsetOrc(id: Reference.Orc): Unit = {
    orcs.update(id.index, None)
  }

  private def setDemon(demon: Demon): Unit = {
    demons.update(demon.id.index, Some(demon))
  }

  private def unsetDemon(id: Reference.Demon): Unit = {
    demons.update(id.index, None)
  }

  private def setBuilding(building: Building): Unit = {
    buildings.update(building.id.index, building)
  }

  private def setGoal(goal: Goal): Unit = {
    goals.update(goal.id.index, Some(goal))
  }

  private def unsetGoal(id: Reference.Goal): Unit = {
    goals.update(id.index, None)
  }

  private def setTileOrc(position: Vec2, orc: Option[Orc]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(orc = orc.map(_.id)))
  }

  private def setTileDemon(position: Vec2, demon: Option[Demon]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(
        demon = demon.map(_.id),
        stock = Stock.Zero))
  }

  private def setTileBuilding(position: Vec2, building: Option[Building]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(building = building.map(_.id)))
  }

  private def setTileGoal(position: Vec2, goal: Option[Goal]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(goal = goal.map(_.id)))
  }

  private def setTile(newTile: Tile): Unit = {
    val index = computeTileIndex(newTile.position)
    tiles.update(
      index,
      newTile)
  }

  private def computeTileIndex(position: Vec2): Int = {
    position.x.toInt + position.y.toInt * Dimensions.MapSize.toInt
  }
}
