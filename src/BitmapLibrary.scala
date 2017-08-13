package offGridOrcs

object BitmapLibrary {
  val InspectCursor = {
    val o = Vec3.Zero
    val x = Colors.CursorFaint
    val Z = Colors.CursorBold
    Bitmap.build(7)(
      o, o, o, x, o, o, o,
      o, o, o, o, o, o, o,
      o, o, Z, Z, Z, o, o,
      x, o, Z, Z, Z, o, x,
      o, o, Z, Z, Z, o, o,
      o, o, o, o, o, o, o,
      o, o, o, x, o, o, o)
  }

  val ZoomedOutCursor = {
    val o = Vec3.Zero
    val x = Colors.CursorFaint
    Bitmap.build(3)(
      x, o, x,
      o, x, o,
      x, o, x)
  }

  val OverlayCursor = {
    val o = Vec3.Zero
    val z = Colors.OverlayCursorFaint
    val Z = Colors.OverlayCursorBold
    Bitmap.build(3)(
      o, z, o,
      z, Z, z,
      o, z, o)
  }

  val MapButton = createButtonBitmap(Colors.OverlayFaintForeground, Colors.OverlayFaintBackground)
    .expand(4 + Glyph.size, 4 + Glyph.size)

  val MapReverseButton = createButtonBitmap(Colors.OverlayFaintReverse, Colors.OverlayFaintBackground)
    .expand(4 + Glyph.size, 4 + Glyph.size)

  val MapPausedOverlay = {
    val pair = Seq(Vec3.Zero, Colors.OverlayPause)
    val pairCount = Dimensions.LowRez.toInt / 2
    val evenRow = Seq.fill(pairCount)(pair).flatten
    val oddRow = Seq.fill(pairCount)(pair.reverse).flatten
    val pixels = Seq.fill(pairCount)(evenRow ++ oddRow).flatten
    Bitmap.build(pairCount * 2)(pixels: _*)
  }

  val InspectScreen = {
    val o = Colors.OverlayBoldBackground
    val Z = Colors.OverlayBoldBackground + Colors.OverlayBoldForeground
    Bitmap.build(5)(
      Z, Z, Z, Z, Z,
      Z, Z, o, Z, Z,
      Z, o, o, o, Z,
      Z, Z, o, Z, Z,
      Z, Z, Z, Z, Z).expand(Dimensions.LowRez.toInt)
  }

  val InspectCorner = {
    val o = Vec3.Zero
    val Z = Colors.OverlayBoldForeground
    Bitmap.build(5)(
      Z, o, o, o, o,
      Z, o, o, o, o,
      Z, o, o, o, o,
      Z, Z, o, o, o,
      o, Z, Z, Z, Z).expand(9)
  }

  val InspectButton = createButtonBitmap(Colors.OverlayBoldForeground, Vec3.Zero)
    .expand(4 + Glyph.size, 4 + Glyph.size)

  val InspectReverseButton = createButtonBitmap(Colors.OverlayBoldReverse, Vec3.Zero)
    .expand(4 + Glyph.size, 4 + Glyph.size)

  val InspectGrid = {
    val size = 3
    val fullSize = size * 3 + 1
    for (selectedY <- 0 until 3; selectedX <- 0 until 3) yield {
      val rangeY = (0 until (size + 1))
        .map(_ + selectedY * size)
      val rangeX = (0 until (size + 1))
        .map(_ + selectedX * size)
      val pixels = for (y <- 0 until fullSize; x <- 0 until fullSize) yield {
        if (rangeY.contains(y) && rangeX.contains(x)) {
          Colors.OverlayBoldForeground
        } else if (y % size == 0 || x % size == 0) {
          Colors.OverlayBoldReverse
        } else {
          Vec3.Zero
        }
      }
      Bitmap.build(fullSize)(pixels: _*)
    }
  }

  def createButtonBitmap(border: Vec3, inside: Vec3): Bitmap = {
    val o = Vec3.Zero
    val z = inside
    val Z = inside + border
    Bitmap.build(3)(
      o, Z, o,
      Z, z, Z,
      o, Z, o)
  }

  // http://www.piskelapp.com/p/agxzfnBpc2tlbC1hcHByEwsSBlBpc2tlbBiAgMDck6XkCQw/view
  val TitleScreen = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAE1UlEQVR4Xu1bzYoTQRDOilFE8OJJEBUJohe96Ct48S18MB/Ek6+wpwUJgiJ48iJ4kIhKDamltvLV33TPJMHsRdNTXV311W9XMme3Xt74u/iP/85OAJw84BQCV3LA3bc3F9/f/drJCsv714a1zdc/w7/8mdfoMz/rkVL0eVWeWXlSOUArawGAACJQqsqg83oDwIYuA4AEQUpKpZH3eN4SeZf2UgSw5CENo+UPAdCMIgD0cwscuc57vPBCNJqHBHU0ABmFK+44JwCeXJbHQQ/oCQKytEycUuhWDzgaAO68Xg6VJgO0BaAFogVCyQOYSUbAKBwqHtCSX6y9kXxuEuwBgGeRVv6ed3iZX8pkAtAqXIR8JHy0f7IQsDpB3fxkBDw0GpQH9uYBU4JDSfbH+82VI8pJcOoQmBIA1IxZ54Wd4PM3T+Hei/OPi2cvnuw889YlMdpLz6P9vE/T0Wf+q1zKUgBsfv7eUXS9/jQAoJ956xqAKl/az2eic2itax9AzMgDSFBmjgTQB0saSyCPRj+zzrQMUPGEtAdcfFgPfCUglhJzA0AGWK0eX2LNBsmEQhcAlrevD4dzHDIAcl3GPMcve5b1jPdnPTACvlQGGU62uOcBTKvDRK5rC1mxHOUQLwQjAEgeDUIXD+iRAyzFGLhMDtIAzBYCUwJAVpMgZBKiVQ0mC4EsALJDy2Z6q/TJUKFcwSVVyyKVRm1+OgQ4IQ1xtC2LPaqALpNSscjtZRkkmT5/+7JTdaMwCAFYvXoESzkd9vDeg+GZPthal4yYhtcYYFJa75ef0f+RDEjoUSFwKeD2ewGrsWldz4DWesaoKkCb+GKEblk9hJqahxcGYQgg4Y75pljuA45F2SjZWV62t5ng1G7fNQlqL9B3eXkX76lYz5xTnghJRSQA6ApKtN5dvCcoWV7ZkCjNBC3lWag5QLCmUB4wo6oASn668xpK5PYqrEGQo6us1Tw65mdNkbIxr+nSHmC1nfKaK0NB0nN7i4SU+cOz7hjvy4RBqg/gwah14UAgSIHlZUWDYM0QkEfx+bLvj2YABIL3XUcaAOl6KNaRxZG7IgCQdXkv3zMyFx/U6kbhNxkAdLBWjIGTFyFS0KKTwmdoxuSB2QDwKoT2HqSIBkAmX9SHZOKfzjkaAEhY9hxdeeizBmFSAEgYGpJaDRJKbFkP0JVkOEt868PeUQmJUX0AH8RKokTlTYF1DqgAIMfhEpBsTshaH4aAVTJ4PC6F0IJ6pSvqEq06j0ZdnjGsrN90F2CmCAR9IKrR1rzOs+hQ0sCczyqZaB7I/LuEACprUgFdtyV9ZH3mo+eEWqneyqergEx2WkgWHlnAm/ONueqiu8hYy7PcqTJIxIcwGdINlBXvGdcvA7AvECo/pqpk/0kBqAidsWKL90WgpENA9wWe4AiAsaB4+yo8u5RBHQZWIhsLQHWfpGcFI28pj8W1pdFv+Xd6AfGShExIlRcv9D6kIAPAvz2O8lTTV2MoBCwX5HUptKb1FEL7PPqhaQKv8iDD6LVSDiALahSRy1UAQB7WAkDksU0AoMQnQdGvySCXQyBKvpnnVnh4idniW/KAqGRJACLa1ucRUFn+JgDeINFi3kuorPAtdKW3xloOOvS9XUPg0JVF8p0AmPrt8TG5ZE5P+gcG1TAbN0hC8QAAAABJRU5ErkJggg=="
}
