package offGridOrcs

object Colors {
  val Forest = Vec3.hexRGB(0x0e5008)
  val ForestHighlight = Forest.lighten(0.12)
  val ForestShadow = Forest.darken(0.12)
  val ForestCover = 0.5
  val Orc = Vec3.hexRGB(0x2cce18)
  val CursorBold = Vec3.One * 0.4
  val CursorFaint = Vec3.One * 0.1
}
