package arx.engine.control.event.Event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 7:35 AM
 */

import arx.Prelude._
import scalaxy.loops._

class Event {
	protected var _consumed = false

	def consume(): Unit = { _consumed = true }
	def isConsumed = _consumed
	def notConsumed = ! _consumed
	def resetConsumed () { _consumed = false }
}
