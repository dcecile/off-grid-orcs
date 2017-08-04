package offGridOrcs

object Main {
  def main(args: Array[String]): Unit = {
    val model = Initialize.model()
    val canvas = SimpleCanvas.createLowRez()
    val loop = Loop(model, canvas)
    Subscribe.window(loop.send(_))
    loop.animate()
  }
}
