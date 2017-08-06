package offGridOrcs

import scala.scalajs.js

object Movement {
  def handleOrcInitialization(orc: Orc, tiles: js.Array[Tile]): Orc = {
    val i = computeIndex(orc.position)
    tiles.update(
      i,
      tiles(i).copy(orc = Some(orc)))
    orc
  }

  def handleOrcMovement(oldOrc: Orc, newOrc: Orc, tiles: js.Array[Tile]): Orc = {
    if (newOrc != oldOrc) {
      val oldPosition = oldOrc.position
      val newPosition = newOrc.position
      if (newPosition != oldPosition) {
        val i = computeIndex(oldPosition)
        tiles.update(
          i,
          tiles(i).copy(orc = None))
      }
      val j = computeIndex(newPosition)
      tiles.update(
        j,
        tiles(j).copy(orc = Some(newOrc)))
    }
    newOrc
  }

  def computeIndex(position: Vec2): Int = {
    position.x.toInt + position.y.toInt * Dimensions.MapSize.toInt
  }
}
