package offGridOrcs

import scala.scalajs.js

final case class SpriteBuffer(size: Int, buffer: js.typedarray.Float64Array) {
  def apply(i: Int) = buffer(i)
}

object SpriteBuffer {
  def build(size: Int)(pixels: Vec3*): SpriteBuffer = {
    val buffer = new js.typedarray.Float64Array(
      (size * size * 3).toInt)
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        val i = x + y * size
        val j = i * 3
        val pixel = pixels(i)
        buffer.update(j + 0, pixel.r)
        buffer.update(j + 1, pixel.g)
        buffer.update(j + 2, pixel.b)
      }
    }
    SpriteBuffer(size, buffer)
  }
}
