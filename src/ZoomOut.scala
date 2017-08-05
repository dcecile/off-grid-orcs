package offGridOrcs

sealed trait ZoomOut {
  val value: Double
}

object ZoomOut {
  final case class OneX() extends ZoomOut {
    val value = 1
  }
  final case class TwoX() extends ZoomOut {
    val value = 2
  }
}
