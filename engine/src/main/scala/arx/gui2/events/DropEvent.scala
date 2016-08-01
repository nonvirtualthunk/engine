package arx.gui2.events

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/1/14
 * Time: 8:52 AM
 */

import arx.Prelude._
import arx.core.vec.ReadVec2f
import arx.engine.control.event.Event.Event
import arx.gui2.Widget

import scalaxy.loops._

/**
 * Drop location is relative to the widget on which it is dropped
 */
case class DropEvent (dragged : Widget, droppedOn : Widget, draggedData : Any, dropLocation: ReadVec2f) extends Event {

}
