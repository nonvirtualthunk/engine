package arx.engine.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 9:58 AM
 */

import arx.engine.EnginePiece
import arx.engine.event.EventBus
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.world.World

class GraphicsEngine(val world : World, val graphicsWorld : GraphicsWorld, val eventBus : EventBus, val gameEventBus : EventBus)
	extends EnginePiece[World, GraphicsComponent]
{
	val self = this
	def graphicsComponents = components

	override def update(deltaSeconds: Float): Unit = {
		super.update(deltaSeconds)
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try { c.getConstructor(classOf[GraphicsEngine]) != null } catch { case e : Exception => false }) match {
			case Some(clazz) => {
				val constructor = clazz.getConstructor(classOf[GraphicsEngine])
				constructor.newInstance(this).asInstanceOf[GraphicsComponent]
			}
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}

	def draw(): Unit = {
		components = components.sortBy(_.drawOrder)
		components.foreach(g => g.draw())
	}
}



object GraphicsEngine {

}