package offGridOrcs

sealed trait UIState

object UIState {
  final case class Title() extends UIState
  final case class Map(camera: Camera) extends UIState
}
