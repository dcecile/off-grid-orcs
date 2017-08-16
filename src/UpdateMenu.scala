package offGridOrcs

object UpdateMenu {
  def update(model: Model.Menu, message: Message): Model = {
    translate(model, message)
  }

  def update(model: Model.Menu, message: MenuMessage): Model = {
    message match {
      case Message.LeftClick(_) =>
        close(model)
      case MenuMessage.Close() =>
        close(model)
      case _ =>
        model
    }
  }

  def translate(model: Model.Menu, message: Message): Model = {
    val newModel = message match {
      case Message.MouseMove(position) =>
        moveMouse(model, Some(position))
      case Message.LeftClick(position) =>
        moveMouse(model, Some(position))
      case Message.MouseLeave() =>
        moveMouse(model, None)
      case _ => model
    }
    val newMessage = message match {
      case Message.KeyDown(key) =>
        translateKeyDown(key).getOrElse(message)
      case _ => message
    }
    update(newModel, newMessage)
  }

  def moveMouse(model: Model.Menu, position: Option[Vec2]): Model.Menu = {
    model.copy(cursor = model.cursor.copy(
      position = position).clamp)
  }

  def translateKeyDown(key: Key): Option[MenuMessage] = {
    key match {
      case Shortcuts.Inspection.Close => Some(
        MenuMessage.Close())
      case _ => None
    }
  }

  def close(model: Model.Menu): Model = {
    model.mode match {
      case Model.MenuMode.Normal() =>
        UpdateMap.moveMouse(
          model.mapModel,
          model.cursor.position)
      case Model.MenuMode.GameOver() =>
        Initialize.initializeModel()
    }
  }
}
