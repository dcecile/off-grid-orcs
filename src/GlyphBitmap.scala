package offGridOrcs

import firstOrc.Glyph
import firstOrc.Font

final case class GlyphBitmap(glyph: Glyph) {
  def char = glyph.char

  lazy val boldBitmap = makeSmallBitmap(
    Colors.OverlayBoldForeground)

  lazy val boldLargeBitmap = makeLargeBitmap(
    Colors.OverlayBoldForeground)

  lazy val boldReverseBitmap = makeSmallBitmap(
    Colors.OverlayBoldReverse)

  lazy val faintBitmap = makeSmallBitmap(
    Colors.OverlayFaintForeground)

  lazy val faintReverseBitmap = makeSmallBitmap(
    Colors.OverlayFaintReverse)

  private def makeSmallBitmap(color: Vec3): Bitmap = {
    val pixels = glyph.bits
      .flatten
      .map(color * _)
    Bitmap.build(Glyph.size)(pixels: _*)
  }

  private def makeLargeBitmap(color: Vec3): Bitmap = {
    val blank = Seq.fill(Glyph.size)(0.0)
    val pixels = glyph.bits
      .zip(glyph.flexs)
      .flatMap({ case (bit, flex) =>
        Seq.fill(flex)(bit ++ blank)
      })
      .flatten
      .map(color * _)
    Bitmap.build(Glyph.size * 2)(pixels: _*)
  }
}

object GlyphBitmap {
  def getSprites(
    topLeft: Vec2,
    string: String,
    bitmap: GlyphBitmap => Bitmap
  ): Seq[Sprite] = {
    val positions = (Stream.iterate(topLeft)
      (_ + Vec2(Glyph.size.toDouble + 1, 0)))
    string
      .zip(positions)
      .map({ case (char, position) =>
        getSprite(position, char, bitmap)
      })
  }

  def getSprite(
    topLeft: Vec2,
    char: Char,
    bitmap: GlyphBitmap => Bitmap
  ): Sprite = {
    Sprite(
      topLeft,
      bitmap(find(char)))
  }

  val glyphBitmaps = Font.glyphs.map(GlyphBitmap(_))
  val unknownGlyphBitmap = GlyphBitmap(Font.unknownGlyph)

  def find(char: Char): GlyphBitmap = {
    glyphBitmaps
      .filter(_.char == char)
      .headOption
      .getOrElse(unknownGlyphBitmap)
  }
}
