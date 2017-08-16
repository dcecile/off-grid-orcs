package offGridOrcs

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html

final case class SimpleCanvas(element: html.Canvas, context: dom.CanvasRenderingContext2D, imageData: dom.ImageData) {
  def drawPixels(colorBuffer: js.typedarray.Float64Array): Unit = {
    element.style.cursor = "none"
    element.style.backgroundImage = "none"
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

  def drawTitle(): Unit = {
    element.style.cursor = "pointer"
    context.clearRect(0, 0, Dimensions.LowRez, Dimensions.LowRez)
    element.style.backgroundImage = s"url(${BitmapLibrary.TitleScreen})"
  }
}

object SimpleCanvas {
  def createLowRez(): SimpleCanvas = {
    val canvasSize = Dimensions.LowRez.toInt
    val elementSize = s"${64 * 6}px"
    val document = dom.document
    val element = document.createElement("canvas").asInstanceOf[html.Canvas]
    element.width = canvasSize
    element.height = canvasSize
    element.style.width = elementSize
    element.style.height = elementSize
    element.style.backgroundSize = s"$elementSize $elementSize"
    // https://builtvisible.com/image-scaling-in-css/
    val pixelatedVariants = Seq(
      ("-ms-interpolation-mode", "nearest-neighbor"), // IE 7+ (non-standard property)
      ("image-rendering", "-webkit-optimize-contrast"), // Safari 6, UC Browser 9.9
      ("image-rendering", "-webkit-crisp-edges"), // Safari 7+
      ("image-rendering", "-moz-crisp-edges"), // Firefox 3.6+
      ("image-rendering", "-o-crisp-edges"), // Opera 12
      ("image-rendering", "pixelated") // Chrome 41+ and Opera 26+
    )
    pixelatedVariants.foreach(
      Function.tupled(element.style.setProperty(_, _)))
    document.body.appendChild(element)
    val context = element.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val imageData = context.getImageData(0, 0, Dimensions.LowRez, Dimensions.LowRez)
    SimpleCanvas(element, context, imageData)
  }
}
