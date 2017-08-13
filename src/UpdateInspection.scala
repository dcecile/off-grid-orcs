package offGridOrcs

object UpdateInspection {
  def update(model: Model.Inspection, message: Message): Model = {
    translate(model, message)
  }

  def update(model: Model.Inspection, message: InspectionMessage): Model = {
    message match {
      case Message.LeftClick(_) =>
        close(model)
      case InspectionMessage.Close() =>
        close(model)
      case InspectionMessage.Move(direction) =>
        model.copy(
          selection = (model.selection + direction).clamp(
            Vec2.Zero, Vec2(2, 2)))
      case InspectionMessage.ChangeMode(newMode) =>
        model.copy(
          mode = newMode)
      case _ =>
        model
    }
  }

  def translate(model: Model.Inspection, message: Message): Model = {
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

  def moveMouse(model: Model.Inspection, position: Option[Vec2]): Model.Inspection = {
    model.copy(cursor = model.cursor.copy(
      position = position).clamp)
  }

  def translateKeyDown(key: Key): Option[InspectionMessage] = {
    key match {
      case Shortcuts.Inspection.Close => Some(
        InspectionMessage.Close())
      case Shortcuts.Inspection.MoveLeft => Some(
        InspectionMessage.Move(Vec2(-1, 0)))
      case Shortcuts.Inspection.MoveRight => Some(
        InspectionMessage.Move(Vec2(+1, 0)))
      case Shortcuts.Inspection.MoveUp => Some(
        InspectionMessage.Move(Vec2(0, -1)))
      case Shortcuts.Inspection.MoveDown => Some(
        InspectionMessage.Move(Vec2(0, +1)))
      case Shortcuts.Inspection.ModeStatus => Some(
        InspectionMessage.ChangeMode(Model.InspectionMode.Status()))
      case Shortcuts.Inspection.ModeStock => Some(
        InspectionMessage.ChangeMode(Model.InspectionMode.Stock()))
      case _ => None
    }
  }

  def close(model: Model.Inspection): Model = {
    UpdateMap.moveMouse(
      saveMode(model),
      model.cursor.position)
  }

  def saveMode(model: Model.Inspection): Model.Map = {
    model.mapModel.copy(
      previousInspectionMode = model.mode)
  }
}
