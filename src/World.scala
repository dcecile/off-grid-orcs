package offGridOrcs

import scala.scalajs.js

final case class World(currentTime: Time, tiles: js.Array[Tile], orcs: js.Array[Orc]) {
  def apply(position: Vec2): Tile = {
    val index = computeTileIndex(position)
    tiles(index)
  }

  def apply(id: Reference.Orc): Orc = {
    orcs(id.index)
  }

  def execute(commands: Seq[Command]): World = {
    commands.foldLeft(this)(_.execute(_))
  }

  def execute(command: Command): World = {
    command match {
      case Command.InsertOrc(position, plan) =>
        val id = Reference.Orc(orcs.length)
        val newOrc = Orc(id, position, plan)
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
    }
  }

  def setOrc(orc: Orc): Unit = {
    orcs.update(orc.id.index, orc)
  }

  def setTileOrc(position: Vec2, orc: Option[Orc]): Unit = {
    val index = computeTileIndex(position)
    tiles.update(
      index,
      tiles(index).copy(orc = orc.map(_.id)))
  }

  def computeTileIndex(position: Vec2): Int = {
    position.x.toInt + position.y.toInt * Dimensions.MapSize.toInt
  }
}
