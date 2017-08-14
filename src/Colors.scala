package offGridOrcs

object Colors {
  val Forest = Vec3.hexRGB(0x0e5008)
  val ForestHardHighlight = Forest.lighten(0.48)
  val ForestSoftHighlight = Forest.lighten(0.12)
  val ForestSoftShadow = Forest.darken(0.12)
  val ForestCover = 0.5
  val Grass = Vec3.hexRGB(0x9ec758)
  val GrassSoftHighlight = Grass.lighten(0.05)
  val GrassSoftShadow = Grass.darken(0.04)
  val GrassHardShadow = Grass.darken(0.15)
  val WoodPile = Vec3.hexRGB(0x726938)
  val BuildingFlooring = Vec3.hexRGB(0x2f2c05)
  val BuildingWalls = Vec3.hexRGB(0xece589)
  val BuildingRoof = Vec3.hexRGB(0xa38d19)
  val BuildingDecal = BuildingRoof.mix(Vec3.hexRGB(0x701400), 0.4)
  val BuildingWallsCover = 0.3
  val BuildingRoofCover = 0.8
  val Orc = Vec3.hexRGB(0x2cce18)
  val CursorBold = Vec3.One * 0.4
  val CursorFaint = Vec3.One * 0.1
  val BlueprintBold = Vec3.hexRGB(0x305080)
  val BlueprintFaint = BlueprintBold.darken(0.7)
  val BlueprintPulseStart = 0.2
  val Goal = Vec3.hexRGB(0x801000)
  val GoalPulseStart = 0.0
  val Overlay = Vec3.hexRGB(0xdbaaff)
  val OverlayFaintForeground = Overlay * 0.3
  val OverlayFaintReverse = Overlay * -0.2
  val OverlayFaintBackground = Overlay * 0.1
  val OverlayBoldForeground = Overlay * 0.6
  val OverlayBoldReverse = Overlay * -0.4
  val OverlayBoldBackground = Overlay * 0.3
  val OverlayCursorBold = Vec3.hexRGB(0xfff578)
  val OverlayCursorFaint = OverlayCursorBold.darken(0.6)
  val OverlayPause = Vec3.hexRGB(0x04061b)
}
