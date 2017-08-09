package offGridOrcs

final case class Blueprint(buildingPositions: Seq[Vec2], clearingPositions: Seq[Vec2], stockpilePositions: Seq[Vec2], cursorSpriteBuffer: SpriteBuffer) {
  def goal(topLeft: Vec2, currentTime: Time)(id: Reference.Goal): Goal = {
    def translate(positions: Seq[Vec2]) = positions.map(topLeft + _)
    val allPositions = Seq(buildingPositions, clearingPositions, stockpilePositions).flatten.distinct
    Goal(
      id = id,
      allPositions = translate(allPositions),
      toClearPositions = translate(allPositions),
      toBuildFloorPositions = translate(buildingPositions),
      toBuildWallPositions = translate(buildingPositions),
      toBuildRoofPositions = translate(buildingPositions),
      stockpilePositions = translate(stockpilePositions),
      startTime = currentTime)
  }
}

object Blueprint {
  sealed trait Element

  object Element {
    final case class Building() extends Element
    final case class Clearing() extends Element
    final case class Stockpile() extends Element
  }

  def build(size: Int)(elements: Option[Element]*): Blueprint = {
    val cursorSpriteBuffer = SpriteBuffer.build(size)(elements map {
      case None =>
        Vec3.Zero
      case Some(Element.Building()) =>
        Colors.BlueprintBold
      case Some(Element.Clearing()) =>
        Colors.BlueprintFaint
      case Some(Element.Stockpile()) =>
        Colors.BlueprintFaint
    }: _*)
    val positionsBuildingClearingStockpile = for (y <- 0 until size; x <- 0 until size) yield {
      val position = Some(Vec2(x.toDouble, y.toDouble))
      val i = x + y * size
      elements(i) match {
        case None =>
          (None, None, None)
        case Some(Element.Building()) =>
          (position, None, None)
        case Some(Element.Clearing()) =>
          (None, position, None)
        case Some(Element.Stockpile()) =>
          (None, None, position)
      }
    }
    val tupleBuildingClearingStockpile = positionsBuildingClearingStockpile.unzip3
    Blueprint(
      tupleBuildingClearingStockpile._1.flatten,
      tupleBuildingClearingStockpile._2.flatten,
      tupleBuildingClearingStockpile._3.flatten,
      cursorSpriteBuffer)
  }

  val Headquarters = {
    val o = None
    val c = Some(Element.Clearing())
    val s = Some(Element.Stockpile())
    val Z = Some(Element.Building())
    build(7)(
      o, c, c, c, c, c, o,
      c, c, Z, Z, Z, c, c,
      c, Z, Z, Z, Z, Z, c,
      c, Z, Z, Z, Z, Z, c,
      s, Z, Z, c, Z, Z, c,
      c, c, Z, c, Z, c, c,
      o, c, c, c, c, c, o)
  }
}
