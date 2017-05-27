package arx.engine.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 9:58 AM
 */

import java.util.concurrent.locks.LockSupport

import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.core.datastructures.UpdateThread
import arx.core.units.UnitOfTime
import arx.engine.EnginePiece
import arx.engine.control.ControlEngine
import arx.engine.control.data.ControlWorld
import arx.engine.data.THasAuxData
import arx.engine.data.THasInternalAuxData
import arx.engine.event.EventBus
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.graphics.data.TGraphicsData
import arx.engine.world.World
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera

class GraphicsEngine(val world : World, val graphicsWorld : GraphicsWorld, val eventBus : EventBus, val gameEventBus : EventBus)
	extends EnginePiece[GraphicsComponent]
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