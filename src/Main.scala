package offGridOrcs

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import org.scalajs.dom
import org.scalajs.dom.html

object Main {
  final case class Vec2(x: Double, y: Double) {
    def +(other: Vec2) =
      Vec2(x + other.x, y + other.y)

    def -(other: Vec2) =
      Vec2(x - other.x, y - other.y)

    def *(other: Vec2) =
      Vec2(x * other.x, y * other.y)

    def *(scale: Double) =
      Vec2(x * scale, y * scale)

    def /(scale: Double) =
      Vec2(x / scale, y / scale)

    def dot(other: Vec2) =
      x * other.x + y * other.y

    def lengthSquared =
      this dot this

    def length =
      Math.sqrt(lengthSquared)
  }

  object Vec2 {
    val zero = Vec2(0, 0)
  }

  final case class Model(locations: js.Array[Location], segment: Int)

  final class Location(val position: Vec2, var shade: Double)

  sealed trait Message
  object Message {
    final case class AnimationFrame(time: Double) extends Message
  }

  val lowRez = 64
  val screenSize = lowRez * 2
  val mapSize = lowRez * 4
  val segments = 1

  def initialLocations(): js.Array[Location] = {
    val maxIndex = mapSize - 1
    val positions = for (x <- 0 to maxIndex; y <- 0 to maxIndex) yield Vec2(x.toDouble, y.toDouble)
    def calculateShade(position: Vec2): Double = {
      val center = Vec2(
        (lowRez.toDouble - 1) / 2,
        (lowRez.toDouble - 1) / 2)
      val distance = (position - center).length
      val period = 8
      val min = 0.3
      val max = 0.7
      ((min + max) / 2
        + (max - min) / 2
          * Math.sin(distance / period * 2 * Math.PI))
    }
    positions
      .map(position =>
        new Location(position, calculateShade(position)))
      .toJSArray
  }

  def updateLocation(location: Location): Unit = {
    location.shade = (location.shade + 0.01) % 1
  }

  def loop(context: dom.CanvasRenderingContext2D, imageData: dom.ImageData, model: Model): Unit = {
    val segmentSize = model.locations.length / segments
    val segmentStart = model.segment * segmentSize
    for (i <- (segmentStart) until (segmentStart + segmentSize)) {
      updateLocation(model.locations(i))
    }
    val buffer = new js.typedarray.Float32Array(lowRez * lowRez)
    val scale = lowRez.toDouble / screenSize.toDouble
    for (i <- 0 until model.locations.length) {
      val location = model.locations(i)
      val position = location.position
      if (position.x < screenSize && position.y < screenSize) {
        val alpha = ((1 - location.shade)
          * scale * scale)
        val i = (position.x * scale).toInt + (position.y * scale).toInt * lowRez
        buffer.update(i, buffer(i) + alpha.toFloat)
      }
    }
    for (i <- 0 until buffer.length) {
      val j = i * 4
      val value = (buffer(i) * 255).toInt
      imageData.data.update(j + 0, value)
      imageData.data.update(j + 1, value)
      imageData.data.update(j + 2, value)
      imageData.data.update(j + 3, 255)
    }
    context.putImageData(imageData, 0, 0)
    dom.window.requestAnimationFrame(_ =>
      loop(context, imageData, model.copy(segment = (model.segment + 1) % segments)))
  }

  def main(args: Array[String]): Unit = {
    val document = dom.document
    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.width = lowRez
    canvas.height = lowRez
    canvas.style.width = "256px"
    canvas.style.height = "256px"
    canvas.style.setProperty("image-rendering", "pixelated")
    document.body.appendChild(canvas)
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val imageData = context.getImageData(0, 0, lowRez.toDouble, lowRez.toDouble)
    val model = Model(initialLocations, 0)
    loop(context, imageData, model)
  }
}
