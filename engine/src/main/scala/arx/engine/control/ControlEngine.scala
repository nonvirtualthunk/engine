package arx.engine.control

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.EnginePiece
import arx.engine.control.components.ControlComponent
import arx.engine.control.data.ControlModes
import arx.engine.control.data.ControlWorld
import arx.engine.control.data.TControlData
import arx.engine.control.event.Event.Event
import arx.engine.control.event.Event.TEventUser
import arx.engine.data.THasInternalAuxData
import arx.engine.event.EventBus
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.world.World

class ControlEngine(val world: World, val graphicsWorld: GraphicsWorld, val controlWorld : ControlWorld, val eventBus: EventBus, val graphicsEventBus: EventBus, val gameEventBus: EventBus)
	extends EnginePiece[ControlComponent]
		with TEventUser {

	val modes = controlWorld[ControlModes]

	onEvent {
		case e : Event => eventBus.fireEvent(e)
	}

	override protected def instantiateComponent(l: List[Class[_]]): AnyRef = {
		l.find(c => try { c.getConstructor(classOf[ControlEngine]) != null } catch { case e : Exception => false }) match {
			case Some(clazz) => {
				val constructor = clazz.getConstructor(classOf[ControlEngine])
				constructor.newInstance(this).asInstanceOf[ControlComponent]
			}
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}
}