package offGridOrcs

import org.scalajs.dom

object Subscribe {
  def subscribeToWindowEvents(send: Message => Unit): Unit = {
    dom.window.onkeydown =
      sendKeyMessage(Message.KeyDown, send)
    dom.window.onkeyup =
      sendKeyMessage(Message.KeyUp, send)
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

  def sendKeyMessage(message: Key => Message, send: Message => Unit)(event: dom.KeyboardEvent): Unit = {
    send(message(Key(event.key)))
  }
}
