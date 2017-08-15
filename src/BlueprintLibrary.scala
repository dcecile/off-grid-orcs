package offGridOrcs

object BlueprintLibrary {
  val Headquarters = {
    val o = None
    val c = Some(Blueprint.Element.Clearing())
    val s = Some(Blueprint.Element.Stockpile())
    val Z = Some(Blueprint.Element.Building())
    val D = Some(Blueprint.Element.Decal())
    Blueprint.build("HQ", 6, 7)(
      o, c, c, c, c, c, o,
      c, c, Z, Z, Z, c, c,
      c, Z, Z, Z, Z, Z, c,
      c, Z, Z, D, Z, Z, c,
      s, Z, Z, c, Z, Z, c,
      c, c, Z, c, Z, c, c,
      o, c, c, c, c, c, o)
  }

  val Home = {
    val o = None
    val c = Some(Blueprint.Element.Clearing())
    val s = Some(Blueprint.Element.Stockpile())
    val Z = Some(Blueprint.Element.Building())
    val D = Some(Blueprint.Element.Decal())
    Blueprint.build("HOME", 4, 7)(
      o, c, c, c, c, c, o,
      c, c, Z, Z, Z, c, c,
      c, Z, Z, Z, Z, Z, c,
      s, Z, Z, Z, Z, Z, c,
      c, c, Z, D, Z, c, c,
      o, c, c, c, c, c, o,
      o, o, o, o, o, o, o)
  }
}
