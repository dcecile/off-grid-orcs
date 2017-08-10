package offGridOrcs

import scala.scalajs.js

final case class World(currentTime: Time, tiles: js.Array[Tile], orcs: js.Array[Orc], goals: js.Array[Goal]) {
  def apply(position: Vec2): Tile = {
    val index = computeTileIndex(position)
    tiles(index)
  }

  def apply(id: Reference.Orc): Orc = {
    orcs(id.index)
  }

  def apply(id: Reference.Goal): Goal = {
    goals(id.index)
  }

  def activeGoals =
    goals.filter(_.isActive)

  def execute(commands: Seq[Command]): World = {
    commands.foldLeft(this)(_.execute(_))
  }

  def execute(command: Command): World = {
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
      case Command.UpdateGoal(newGoal) =>
        setGoal(newGoal)
        if (!newGoal.isActive) {
          for (position <- newGoal.allPositions) {
            setTileGoal(position, None)
          }
        }
        this
      case Command.UpdateTile(newTile) =>
        setTile(newTile)
        this
    }
  }

  def setOrc(orc: Orc): Unit = {
    orcs.update(orc.id.index, orc)
  }

  def setGoal(goal: Goal): Unit = {
    goals.update(goal.id.index, goal)
  }

  def setTileOrc(position: Vec2, orc: Option[Orc]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(orc = orc.map(_.id)))
  }

  def setTileGoal(position: Vec2, goal: Option[Goal]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(goal = goal.map(_.id)))
  }

  def setTile(newTile: Tile): Unit = {
    val index = computeTileIndex(newTile.position)
    tiles.update(
      index,
      newTile)
  }

  def computeTileIndex(position: Vec2): Int = {
    position.x.toInt + position.y.toInt * Dimensions.MapSize.toInt
  }
}
