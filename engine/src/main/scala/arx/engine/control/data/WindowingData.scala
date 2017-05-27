package arx.engine.control.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.Desktop

import scala.collection.immutable.Stack
import scalaxy.loops._

class WindowingData extends TControlData {
	var desktop : Desktop = new Desktop
	var focusedWidget : Option[Widget] = None
	var lastWidgetUnderMouse : Option[Widget] = None
	var currentPressedWidget : Option[Widget] = None
	var draggingWidget : Option[Widget] = None
	var modalWidgetStack = Stack[ModalWidget]()
}

case class ModalWidget (widget : Widget, closeOnClickElsewhere : Boolean)