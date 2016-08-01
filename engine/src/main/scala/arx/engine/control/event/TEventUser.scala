package arx.engine.control.event.Event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 7:36 AM
 */

import arx.Prelude
import arx.Prelude._
import scala.collection.mutable
import scalaxy.loops._

trait TEventUser {
	@transient var _eventListeners : List[(PartialFunction[Event,_],Boolean, Int)] = Nil
	def eventListeners : List[(PartialFunction[Event,_],Boolean, Int)] = if ( _eventListeners != null ) { _eventListeners } else { Nil }
	def eventListeners_= ( e : List[(PartialFunction[Event,_],Boolean, Int)] ) { _eventListeners = e }

	def connectEventsTo ( forwardTo : TEventUser ) {
		this.onEvent {
			case e : Event => forwardTo.handleEvent(e); false
		}
	}

	def addNextEventUser ( fallback : TEventUser ): Unit = {
		eventListeners ::= ({
			case e : Event => fallback.handleEvent(e)
		}, false, 1)
	}

	def onEvent ( func: PartialFunction[Event,_] ){
		eventListeners ::= (func,false,0)
	}
	def consumeEvent ( func : PartialFunction[Event,_] ){
		eventListeners ::= (func,true,0)
	}
	def eventFallback ( func : PartialFunction[Event,_]): Unit = {
		eventListeners ::= (func,false,1)
	}

	@deprecated
	def fireEvent (event : Event) : Boolean = handleEvent(event)

	def handleEvent ( event: Event ) : Boolean = {
		TEventUser.pushEvent(event)
		// Sort the event listeners in order of precedence
		if (_eventListeners != null) {
			_eventListeners = _eventListeners.sortBy(_._3)
		}

		for ( (listener,consumeByDefault,_: Int) <- eventListeners ; if event.notConsumed ) {
			if ( listener.isDefinedAt(event) ){
				val ret = listener(event)
				val shouldConsume = ret match {
					case b : Boolean => b
					case u : Unit => consumeByDefault//ok, no return, that's fine
					case a => consumeByDefault
				}
				if ( shouldConsume && event.notConsumed ) {
					event.consume()
				} else {  }
			}
		}

		event.resetConsumed()
		TEventUser.popEvent(event)
		true
	}

	protected def consume() { TEventUser.activeEvent.consume() }
	protected def activeEvent = TEventUser.activeEvent
}


object TEventUser {
	var eventStack = new ThreadLocal[mutable.Stack[Event]]{
		override def initialValue() = mutable.Stack[Event]()
	}

	def pushEvent (evt:Event) { eventStack.get.push(evt) }
	def popEvent (evt:Event) { val ret = eventStack.get.pop(); Prelude.posit(ret eq evt,"Event pop mismatch, " + ret + " , " + evt) }
	def activeEvent = eventStack.get.top

}