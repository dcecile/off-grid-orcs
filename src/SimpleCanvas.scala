package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html

final case class SimpleCanvas(element: html.Canvas, context: dom.CanvasRenderingContext2D, imageData: dom.ImageData) {
  def drawPixels(shades: js.typedarray.Float64Array): Unit = {
    val output = imageData.data
    for (i <- 0 until shades.length) {
      val j = i * 4
      val value = (shades(i) * 255).toInt
      output.update(j + 0, value)
      output.update(j + 1, value)
      output.update(j + 2, value)
      output.update(j + 3, 255)
    }
    context.putImageData(imageData, 0, 0)
  }
}

object SimpleCanvas {
  def createLowRez(): SimpleCanvas = {
    val document = dom.document
    val element = document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = Dimensions.lowRez.toInt
    element.height = Dimensions.lowRez.toInt
    element.style.width = "256px"
    element.style.height = "256px"
    element.style.setProperty("image-rendering", "pixelated")
    document.body.appendChild(element)
    val context = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val imageData = context.getImageData(0, 0, Dimensions.lowRez, Dimensions.lowRez)
    SimpleCanvas(element, context, imageData)
  }
}
