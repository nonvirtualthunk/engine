package arx.engine.control.components.windowing.events

/**
  * TODO: Add javadoc
  */

import arx.core.vec.ReadVec2f
import arx.engine.control.components.windowing.Widget
import arx.engine.control.event.Event.UIEvent

case class DropEvent(draggedWidget: Widget, droppedOn: Widget, localPos: ReadVec2f) extends UIEvent

case class FocusGainedEvent(focusedWidget: Widget) extends UIEvent

case class FocusLostEvent(previouslyFocusedWidget: Widget) extends UIEvent

case class RequestFocusEvent(focusedWidget : Widget) extends UIEvent