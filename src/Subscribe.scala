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
  val scrollSpeed = 3.0

  def translateKeyDown(event: dom.KeyboardEvent): Option[Message] = {
    event.key match {
      case ScrollLeft => Some(
        Message.StartScrollX(-scrollSpeed))
      case ScrollRight => Some(
        Message.StartScrollX(+scrollSpeed))
      case ScrollUp => Some(
        Message.StartScrollY(-scrollSpeed))
      case ScrollDown => Some(
        Message.StartScrollY(+scrollSpeed))
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
