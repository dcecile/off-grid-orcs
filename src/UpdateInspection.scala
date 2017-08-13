package offGridOrcs

object UpdateInspection {
  def update(model: Model.Inspection, message: Message): Model = {
    message match {
      case Message.LeftClick(position) =>
        UpdateMap.update(
          model.mapModel,
          Message.MouseMove(position))
      case _ =>
        model
    }
  }
}
