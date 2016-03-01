package arx.engine.event

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 2:34 PM
  */

import java.util.concurrent.ConcurrentLinkedDeque

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._

class EventBus {
	val sizeLimit = 1000
	var events = new ConcurrentLinkedDeque[GameEvent]()
	var listeners = List[PartialFunction[_ >: GameEvent,Unit]]()

	def fireEvent(event : GameEvent): Unit = {
		listeners.filter(l => l.isDefinedAt(event)).foreach(_.apply(event))
		events.addLast(event)
		while (events.size() > sizeLimit) {
			events.removeFirst()
		}
	}

	def onEvent[T >: GameEvent] (listener: PartialFunction[T,Unit]): Unit = {
		listeners ::= listener
	}
}
