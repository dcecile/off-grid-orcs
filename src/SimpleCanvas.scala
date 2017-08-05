package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html

final case class SimpleCanvas(element: html.Canvas, context: dom.CanvasRenderingContext2D, imageData: dom.ImageData) {
  def drawPixels(colorBuffer: js.typedarray.Float64Array): Unit = {
    val output = imageData.data
    for (i <- 0 until colorBuffer.length / 3) {
      val j = i * 3
      val k = i * 4
      val r = (colorBuffer(j + 0) * 255).toInt
      val g = (colorBuffer(j + 1) * 255).toInt
      val b = (colorBuffer(j + 2) * 255).toInt
      output.update(k + 0, r)
      output.update(k + 1, g)
      output.update(k + 2, b)
      output.update(k + 3, 255)
    }
    context.putImageData(imageData, 0, 0)
  }
}

object SimpleCanvas {
  def createLowRez(): SimpleCanvas = {
    val document = dom.document
    val element = document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = Dimensions.LowRez.toInt
    element.height = Dimensions.LowRez.toInt
    element.style.width = "384px"
    element.style.height = "384px"
    element.style.setProperty("image-rendering", "pixelated")
    document.body.appendChild(element)
    val context = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val imageData = context.getImageData(0, 0, Dimensions.LowRez, Dimensions.LowRez)
    SimpleCanvas(element, context, imageData)
  }
}
