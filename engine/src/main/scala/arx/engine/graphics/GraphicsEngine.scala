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
import arx.engine.data.THasAuxData
import arx.engine.data.THasInternalAuxData
import arx.engine.event.EventBus
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.graphics.data.TGraphicsData
import arx.engine.world.World
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera

class GraphicsEngine(val world : World, val eventBus : EventBus, val gameEventBus : EventBus)
	extends EnginePiece[GraphicsComponent]
		with THasInternalAuxData[TGraphicsData]
{
	val self = this
	def graphicsComponents = components
	var pov : TCamera = new EyeCamera()

	addComponent[PovComponent]

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
		pov.update(0.016666667.seconds)
		components = components.sortBy(_.drawPriority * -1)
		components.foreach(g => g.draw())
	}

	def createUpdateThread(i : Int) : KillableThread = {
		val localComponents = components.zipWithIndex.filter {
			case(comp, index) => index % parallelism == i
		}.unzip.left

		new KillableThread(Killable.ApplicationLevel) {
			val minInterval = 0.004.seconds.inNanoseconds.toLong

			var lastUpdated = System.nanoTime()
			override def whileRunningDo(): Unit = {
				val curTime = System.nanoTime()
				val deltaTime = curTime - lastUpdated
				if (deltaTime > minInterval) {
					lastUpdated = curTime

					for (comp <- localComponents){
						val subDelta = (curTime - comp.lastUpdated).toDouble.nanoseconds
						if (subDelta > comp.minimumUpdateInterval) {
							comp.updateSelf(subDelta)
							comp.lastUpdated = curTime
						}
					}
//					localComponents.foreach(c => c.updateSelf(deltaTime.toDouble.nanoseconds))
				} else {
					LockSupport.parkNanos(2000000) // 5 milis
				}
			}

			start()
		}
	}

	protected lazy val updateThreads = fillList(parallelism)(i => createUpdateThread(i))

	override def initialize(): Unit = {
		super.initialize()
		updateThreads.size // trigger the lazy evaluation
	}
}


class PovComponent(graphicsEngine : GraphicsEngine) extends GraphicsComponent(graphicsEngine) {
	minimumUpdateInterval = 0.0.seconds
	override def draw(): Unit = {
//		val cur = System.nanoTime()
//		val delta = (cur - lastUpdated).toDouble.nanoseconds
//		lastUpdated = cur
//		graphicsEngine.pov.update(delta)
	}

	override protected def update(dt: UnitOfTime): Unit = {
//		graphicsEngine.pov.update(dt)
	}
}



object GraphicsEngine {

}