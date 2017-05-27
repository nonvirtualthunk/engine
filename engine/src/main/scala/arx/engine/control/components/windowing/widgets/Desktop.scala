package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.event.Event.Event

import scalaxy.loops._

class Desktop extends Widget(null) {
	identifier = "Desktop"

	override def handleEvent(event: Event): Boolean = false

	override def selfAndAncestors: List[Widget] = List(this)
}
