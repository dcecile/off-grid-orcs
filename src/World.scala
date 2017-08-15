package offGridOrcs

import scala.scalajs.js

final case class World(currentTime: Time, tiles: js.Array[Tile], orcs: js.Array[Orc], buildings: js.Array[Building], goals: js.Array[Option[Goal]]) {
  def apply(position: Vec2): Tile = {
    val index = computeTileIndex(position)
    tiles(index)
  }

  def apply(id: Reference.Orc): Orc = {
    orcs(id.index)
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

  def activeGoals: Seq[Goal] =
    goals.flatten

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
          val newBuilding = Building.fromBlueprint(newBuildingID, oldGoal.topLeft, oldGoal.blueprint)
          setBuilding(newBuilding)
          for (position <- newBuilding.positions) {
            setTileBuilding(position, Some(newBuilding))
          }
      }
    }
    this
  }

  private def setOrc(orc: Orc): Unit = {
    orcs.update(orc.id.index, orc)
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
