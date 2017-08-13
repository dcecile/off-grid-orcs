package offGridOrcs

import scala.scalajs.js

final case class Bitmap(size: Int, buffer: js.typedarray.Float64Array) {
  def apply(i: Int) = buffer(i)

  def expand(newSize: Int): Bitmap = {
    expand(newSize, newSize)
  }

  def expand(width: Int, height: Int): Bitmap = {
    val newSize = width max height
    val newBuffer = new js.typedarray.Float64Array(
      (newSize * newSize * 3).toInt)
    val border = (size - 1) / 2
    for (y <- 0 until height) {
      val sourceY = if (y < border) {
        y
      } else if (y >= height - border) {
        y - (height - size)
      } else {
        size / 2
      }
      for (x <- 0 until width) {
        val sourceX = if (x < border) {
          x
        } else if (x >= width - border) {
          x - (width - size)
        } else {
          size / 2
        }
        val i = (sourceX + sourceY * size) * 3
        val j = (x + y * newSize) * 3
        newBuffer.update(j + 0, buffer(i + 0))
        newBuffer.update(j + 1, buffer(i + 1))
        newBuffer.update(j + 2, buffer(i + 2))
      }
    }
    Bitmap(newSize, newBuffer)
  }
}

object Bitmap {
  def build(size: Int)(pixels: Vec3*): Bitmap = {
    if (pixels.length != size * size) {
      throw new IllegalArgumentException(s"Incorrect number of pixels: ${pixels.length}")
    }
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
    Bitmap(size, buffer)
  }
}
