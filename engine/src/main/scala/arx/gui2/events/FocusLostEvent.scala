package arx.gui2.events

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/14
 * Time: 12:35 PM
 */

import arx.Prelude._
import arx.engine.control.event.Event.UIEvent

import scalaxy.loops._

class FocusLostEvent extends UIEvent {}
class FocusGainedEvent extends UIEvent {}