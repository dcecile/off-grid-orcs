package offGridOrcs

import scala.scalajs.js

final case class Model(currentTime: Time, tiles: js.Array[Tile], orc: Orc, uiState: UIState)
