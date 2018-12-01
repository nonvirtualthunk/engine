package arx.engine.advanced.lenginepieces

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:20 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.engine.EnginePiece
import arx.engine.advanced.lenginecomponents.LControlComponent
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.data.{ControlModes, ControlWorld}
import arx.engine.control.event.Event.{Event, TEventUser}
import arx.engine.event.EventBus
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.lworld.LWorld
import arx.engine.world.World

class LControlEngine(val world: LWorld, val graphicsWorld: GraphicsWorld, val controlWorld : ControlWorld, val eventBus: EventBus, val graphicsEventBus: EventBus, val gameEventBus: EventBus)
	extends EnginePiece[LWorld, LControlComponent]
		with TEventUser {

	val modes = controlWorld[ControlModes]

	onEvent {
		case e : Event => eventBus.fireEvent(e)
	}

	override protected def instantiateComponent(l: List[Class[_]]): AnyRef = {
		l.find(c => try { c.getConstructor(classOf[LControlEngine]) != null } catch { case e : Exception => false }) match {
			case Some(clazz) => {
				val constructor = clazz.getConstructor(classOf[LControlEngine])
				constructor.newInstance(this).asInstanceOf[LControlComponent]
			}
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}
}