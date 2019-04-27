package arx.engine.advanced.lenginepieces

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:19 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.engine.EnginePiece
import arx.engine.advanced.lenginecomponents.LGraphicsComponent
import arx.engine.event.EventBus
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.lworld.{GameEventClock, LWorld, LWorldView}
import arx.engine.world.World

class LGraphicsEngine(protected[engine] val world : LWorld, val graphicsWorld : GraphicsWorld, val eventBus : EventBus, val gameEventBus : EventBus)
	extends EnginePiece[LWorldView, LGraphicsComponent]
{
	val self = this
	val view = world.viewAtTime(GameEventClock(0))
	def graphicsComponents = components

	def worldCore = world

	override def update(deltaSeconds: Float): Unit = {
		super.update(deltaSeconds)
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try { c.getConstructor(classOf[LGraphicsEngine]) != null } catch { case e : Exception => false }) match {
			case Some(clazz) => {
				val constructor = clazz.getConstructor(classOf[LGraphicsEngine])
				constructor.newInstance(this).asInstanceOf[LGraphicsComponent]
			}
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}

	def draw(): Unit = {
		components = components.sortBy(_.drawOrder)
		components.foreach(g => g.draw())
	}
}
