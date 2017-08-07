package offGridOrcs

import org.scalajs.dom

object Subscribe {
  def subscribeToWindowEvents(send: Message => Unit): Unit = {
    dom.window.onkeydown =
      sendKeyMessage(translateKeyDown, send)
    dom.window.onkeyup =
      sendKeyMessage(translateKeyUp, send)
  }

  def subscribeToCanvasEvents(canvas: SimpleCanvas, send: Message => Unit): Unit = {
    canvas.element.onclick = { event: dom.MouseEvent =>
      if (event.button == 0) {
        sendMouseMessage(Message.LeftClick, send)(event)
      }
    }
    canvas.element.onmousemove =
      sendMouseMessage(Message.MouseMove, send)
    canvas.element.onmouseleave = { _ =>
      send(Message.MouseLeave())
    }
  }

  def sendMouseMessage(message: Vec2 => Message, send: Message => Unit)(event: dom.MouseEvent): Unit = {
    val targetElement = event.target.asInstanceOf[dom.raw.HTMLElement]
    val clientRect = targetElement.getBoundingClientRect
    val clientPosition = Vec2(
      (event.clientX - clientRect.left) / clientRect.width,
      (event.clientY - clientRect.top) / clientRect.height)
    val messagePosition = (clientPosition * Dimensions.LowRez).floor
    send(message(messagePosition))
  }

  def sendKeyMessage(translate: dom.KeyboardEvent => Option[Message], send: Message => Unit)(event: dom.KeyboardEvent): Unit = {
    translate(event).foreach(send(_))
  }

  val ScrollLeft = "ArrowLeft"
  val ScrollRight = "ArrowRight"
  val ScrollUp = "ArrowUp"
  val ScrollDown = "ArrowDown"
  val ScrollSpeed = 1.0
  val ZoomIn = "c"
  val ZoomOut = "x"
  val Reset = "Backspace"

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
      case Reset => Some(
        Message.Reset())
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
