package arx.engine.event

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 2:36 PM
  */

import arx.Prelude._
import arx.engine.control.event.Event.Event
import scalaxy.loops._
import arx.core.vec._

class GameEvent extends Event {
	val createdAt = curTime()
}
