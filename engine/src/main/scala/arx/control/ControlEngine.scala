package arx.control

/**
  * TODO: Add javadoc
  */

import arx.engine.control.event.Event.Event
import arx.engine.control.event.Event.TEventUser
import arx.engine.event.EventBus
import arx.engine.world.World
import arx.gui2.WindowingSystem2
import arx.Prelude._
import arx.application.Noto
import arx.engine.graphics.GraphicsEngine

import scala.collection.immutable.Stack

class ControlEngine(val world: World, val graphicsBus: EventBus, val gameBus: EventBus, val graphicsEngine : GraphicsEngine, val windowingSystem: WindowingSystem2)
	extends TEventUser {
	var modeStack = Stack[ControlMode]()

	def update(deltaSeconds: Float): Unit = {
		for (mode <- modeStack) {
			mode.update(deltaSeconds.seconds)
		}
	}


	onEvent {
		case e : Event =>
			for (mode <- modeStack.headOption if e.notConsumed) {
				if (mode.handleEvent(e)) {
					e.consume()
				}
			}
	}


	def popMode(mode: ControlMode): Unit = {
		if (modeStack.headOption.contains(mode)) {
			modeStack = modeStack.pop
			mode.deactivate()
		} else {
			Noto.warn("Attempted to pop mode " + mode + " from the mode stack, but it was not topmost")
		}
	}

	def pushMode(mode : ControlMode): Unit = {
		modeStack = modeStack push mode
		mode.activate()
	}
}
