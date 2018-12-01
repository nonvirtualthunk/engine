package arx.engine.advanced.lenginepieces

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/17/18
  * Time: 8:00 AM
  */

import arx.engine.EnginePiece
import arx.engine.advanced.lenginecomponents.LGameComponent
import arx.engine.event.EventBus
import arx.engine.game.events.{LEntityAdded, LEntityRemoved}
import arx.engine.lworld.LWorld

class LGameEngine (val world: LWorld, val eventBus : EventBus) extends EnginePiece[LWorld, LGameComponent] {
	val self = this

	parallelism = 4

	override def initialize(serial: Boolean): Unit = {
		super.initialize(serial)

		world.onEntityAddedCallbacks ::= (e => eventBus.fireEvent(LEntityAdded(e)))
		world.onEntityRemovedCallbacks ::= (e => eventBus.fireEvent(LEntityRemoved(e)))
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try {c.getConstructor(classOf[LGameEngine]) != null} catch {case e: Exception => false}) match {
			case Some(clazz) =>
				val constructor = clazz.getConstructor(classOf[LGameEngine])
				constructor.newInstance(this).asInstanceOf[LGameComponent]
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}
}