package offGridOrcs

import org.scalajs.dom

object Subscribe {
  def window(send: Function[Message, Unit]): Unit = {
    dom.window.onkeydown =
      translateKeyToSend(translateKeyDown, send)
    dom.window.onkeyup =
      translateKeyToSend(translateKeyUp, send)
  }

  def translateKeyToSend(translate: Function[dom.KeyboardEvent, Option[Message]], send: Function[Message, Unit])(event: dom.KeyboardEvent): Unit = {
    translate(event).foreach(send(_))
  }

  val ScrollLeft = "ArrowLeft"
  val ScrollRight = "ArrowRight"
  val ScrollUp = "ArrowUp"
  val ScrollDown = "ArrowDown"
  val ScrollSpeed = 1.0
  val ZoomIn = "c"
  val ZoomOut = "x"

  def translateKeyDown(event: dom.KeyboardEvent): Option[Message] = {
    event.key match {
      case ScrollLeft => Some(
        Message.StartScrollX(-ScrollSpeed))
      case ScrollRight => Some(
        Message.StartScrollX(+ScrollSpeed))
      case ScrollUp => Some(
        Message.StartScrollY(-ScrollSpeed))
      case ScrollDown => Some(
        Message.StartScrollY(+ScrollSpeed))
      case ZoomIn => Some(
        Message.ZoomIn())
      case ZoomOut => Some(
        Message.ZoomOut())
      case _ => None
    }
  }

  def translateKeyUp(event: dom.KeyboardEvent): Option[Message] = {
    event.key match {
      case ScrollLeft => Some(
        Message.StopScrollX())
      case ScrollRight => Some(
        Message.StopScrollX())
      case ScrollUp => Some(
        Message.StopScrollY())
      case ScrollDown => Some(
        Message.StopScrollY())
      case _ => None
    }
  }
}
