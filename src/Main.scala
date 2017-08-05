package offGridOrcs

object Main {
  def main(args: Array[String]): Unit = {
    val model = Initialize.initializeModel()
    val canvas = SimpleCanvas.createLowRez()
    val loop = new Loop(model, canvas)
    Subscribe.subscribeToWindowEvents(loop.send(_))
    Subscribe.subscribeToCanvasEvents(canvas, loop.send(_))
    loop.send(Message.Animate())
  }
}
