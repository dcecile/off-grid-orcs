package offGridOrcs

object Update {
  def updateModel(model: Model, message: Message): Model = {
    model match {
      case titleModel: Model.Title =>
        updateTitleModel(titleModel, message)
      case mapModel: Model.Map =>
        UpdateMap.update(mapModel, message)
      case inspectionModel: Model.Inspection =>
        UpdateInspection.update(inspectionModel, message)
      case menuModel: Model.Menu =>
        UpdateMenu.update(menuModel, message)
    }
  }

  def updateTitleModel(model: Model.Title, message: Message): Model = {
    message match {
      case Message.LeftClick(position) =>
        Initialize.initializeMapModel(position)
      case _ =>
        model
    }
  }
}
