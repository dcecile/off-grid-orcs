package offGridOrcs

final case class Glyph(char: Char, bits: Seq[Seq[Double]], flexs: Seq[Int]) {
  lazy val boldBitmap = makeSmallBitmap(
    Colors.OverlayBoldForeground)

  lazy val boldLargeBitmap = makeLargeBitmap(
    Colors.OverlayBoldForeground)

  lazy val reverseBitmap = makeSmallBitmap(
    Colors.OverlayBoldReverse)

  private def makeSmallBitmap(color: Vec3): Bitmap = {
    val pixels = bits
      .flatten
      .map(color * _)
    Bitmap.build(Glyph.size)(pixels: _*)
  }

  private def makeLargeBitmap(color: Vec3): Bitmap = {
    val blank = Seq.fill(Glyph.size)(0.0)
    val pixels = bits
      .zip(flexs)
      .flatMap({ case (bit, flex) =>
        Seq.fill(flex)(bit ++ blank)
      })
      .flatten
      .map(color * _)
    Bitmap.build(Glyph.size * 2)(pixels: _*)
  }
}

object Glyph {
  val size = 6

  def build(char: Char)(
    blank0: String,
    row0: String, flex0: Int,
    row1: String, flex1: Int,
    row2: String, flex2: Int,
    row3: String, flex3: Int,
    row4: String, flex4: Int,
    row5: String, flex5: Int,
    blank1: String
  ): Glyph = {
    val rows = Seq(row0, row1, row2, row3, row4, row5)
    val flexs = Seq(flex0, flex1, flex2, flex3, flex4, flex5)
    if (flexs.sum != size * 2) {
      throw new IllegalArgumentException(s"Wrong flex sum: $char ${flexs.sum}")
    }
    for (row <- rows) {
      row match {
        case parser(bits) => ()
        case _ => throw new IllegalArgumentException(s"Wrong row format: $char $row")
      }
    }
    val bits = rows
      .map(_.substring(2, 2 + size * 2))
      .map(row => (0 until (size * 2) by 2)
        .map(row(_))
        .map(_ match {
          case '[' => 1.0
          case ' ' => 0.0
        }))
    Glyph(char, bits, flexs)
  }

  val parser = """^  (  |\[\]){6}  $""".r

  def getSprites(topLeft: Vec2, string: String, bitmap: Glyph => Bitmap): Seq[Sprite] = {
    val positions = (Stream.iterate(topLeft)
      (_ + Vec2(Glyph.size.toDouble + 1, 0)))
    string
      .zip(positions)
      .map({ case (char, position) =>
        getSprite(position, char, bitmap)
      })
  }

  def getSprite(topLeft: Vec2, char: Char, bitmap: Glyph => Bitmap): Sprite = {
    Sprite(topLeft, bitmap(find(char)))
  }

  def find(char: Char): Glyph = {
    GlyphLibrary.glyphs
      .filter(_.char == char)
      .headOption
      .getOrElse(GlyphLibrary.unknownGlyph)
  }
}
