package offGridOrcs

final case class Tile(position: Vec2, shade: Shade, orc: Option[Reference.Orc], goal: Option[Reference.Goal])
