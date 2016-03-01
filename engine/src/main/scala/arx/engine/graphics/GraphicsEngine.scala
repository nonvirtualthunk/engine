package arx.engine.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 9:58 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.datastructures.UpdateThread
import arx.core.units.UnitOfTime
import arx.engine.EnginePiece
import arx.engine.event.EventBus
import arx.engine.graphics.GraphicsEngine.PovComponent
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.World
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera

class GraphicsEngine(world : World, val eventBus : EventBus) extends EnginePiece[GraphicsComponent] {
	def graphicsComponents = components
	var pov : TCamera = new EyeCamera()

	addComponent[PovComponent]

	override def update(deltaSeconds: Float): Unit = {
		super.update(deltaSeconds)
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try { c.getConstructor(classOf[GraphicsEngine], classOf[World]) != null } catch { case e : Exception => false }) match {
			case Some(clazz) => {
				val constructor = clazz.getConstructor(classOf[GraphicsEngine], classOf[World]);
				constructor.newInstance(this, world).asInstanceOf[GraphicsComponent]
			}
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}

	def draw(): Unit = {
		pov.update(0.016666667.seconds)
		components = components.sortBy(_.drawPriority * -1)
		components.foreach(g => g.draw())
	}
}



object GraphicsEngine {
	class PovComponent(engine : GraphicsEngine, world : World) extends GraphicsComponent(engine,world) {
		override def update(dt: UnitOfTime): Unit = {

		}

		override def draw(): Unit = {}
	}
}