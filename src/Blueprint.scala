package offGridOrcs

final case class Blueprint(
  name: String,
  housingCapacity: Int,
  cursorBitmap: Bitmap,
  buildingPositions: Seq[Vec2],
  decalPosition: Vec2,
  clearingPositions: Seq[Vec2],
  stockpilePosition: Vec2
) {
  val isHeadquarters = name == "HQ"
}

object Blueprint {
  sealed trait Element

  object Element {
    final case class Building() extends Element
    final case class Decal() extends Element
    final case class Clearing() extends Element
    final case class Stockpile() extends Element
  }

  def build(name: String, housingCapacity: Int, size: Int)(elements: Option[Element]*): Blueprint = {
    val cursorBitmap = Bitmap.build(size)(elements map {
      case None =>
        Vec3.Zero
      case Some(Element.Building()) =>
        Colors.BlueprintBold
      case Some(Element.Decal()) =>
        Colors.BlueprintBold
      case Some(Element.Clearing()) =>
        Colors.BlueprintFaint
      case Some(Element.Stockpile()) =>
        Colors.BlueprintFaint
    }: _*)
    val positionTuples = for (y <- 0 until size; x <- 0 until size) yield {
      val position = Some(Vec2(x.toDouble, y.toDouble))
      val i = x + y * size
      elements(i) match {
        case None =>
          (None, None, None, None)
        case Some(Element.Building()) =>
          (position, None, position, None)
        case Some(Element.Decal()) =>
          (position, position, position, None)
        case Some(Element.Clearing()) =>
          (None, None, position, None)
        case Some(Element.Stockpile()) =>
          (None, None, position, position)
      }
    }
    Blueprint(
      name,
      housingCapacity,
      cursorBitmap,
      buildingPositions = positionTuples.flatMap(_._1),
      decalPosition = positionTuples.flatMap(_._2).head,
      clearingPositions = positionTuples.flatMap(_._3),
      stockpilePosition = positionTuples.flatMap(_._4).head)
  }
}
