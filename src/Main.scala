package offGridOrcs

object Main {
  def main(args: Array[String]): Unit = {
    val loop = Loop(
      Initialize.model(),
      SimpleCanvas.createLowRez())
    loop.animate()
  }
}
