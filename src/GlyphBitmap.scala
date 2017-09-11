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
    makeBitmap(glyph.bits, color, Glyph.size)
  }

  private def makeLargeBitmap(color: Vec3): Bitmap = {
    val blank = Seq.fill(Glyph.size)(false)
    val bits = glyph.tallBits
      .map(_ ++ blank)
    makeBitmap(bits, color, Glyph.size * 2)
  }

  private def makeBitmap(bits: Seq[Seq[Boolean]], color: Vec3, size: Int): Bitmap = {
    val pixels = bits
      .flatten
      .map(colorBit(color, _))
    Bitmap.build(size)(pixels: _*)
  }

  private def colorBit(color: Vec3, bit: Boolean): Vec3 = {
    if (bit) {
      color
    } else {
      Vec3.Zero
    }
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
