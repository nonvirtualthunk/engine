package arx.engine.game

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:11 PM
 */

import arx.core.datastructures.UpdateThread
import arx.core.units.UnitOfTime
import arx.engine.EnginePiece
import arx.engine.game.components.GameComponent
import arx.engine.world.World
import arx.Prelude._
import scalaxy.loops._

class GameEngine(val world: World) extends EnginePiece[GameComponent] {
	val self = this

	parallelism = 4
	def graphicsComponents = components

	addComponent[GameEngine.TimeComponent]

	protected def instantiateComponent(l: List[Class[_]]): AnyRef = {
		l.find(c => try {c.getConstructor(classOf[GameEngine], classOf[World]) != null} catch {case e: Exception => false}) match {
			case Some(clazz) =>
				val constructor = clazz.getConstructor(classOf[GameEngine], classOf[World]);
				constructor.newInstance(this, world).asInstanceOf[GameComponent]
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}

	override def createUpdateThread(i: Int): UpdateThread = {
		val world = self.world
		new UpdateThread(0.01.seconds) {
			val timeData = world.timeData
			// divide up the components such that each graphics thread has its own share
			val localComponents = components.zipWithIndex.filter {
				case(comp, index) => index % parallelism == i
			}.unzip.left

			override def update(): Unit = {
				if (timeData.timeAcceleration > 1.0f) { // we're "physics warping" in ksp parlance, run multiple updates
					for (i <- 0 until timeData.timeAcceleration.toInt optimized) {
						localComponents.foreach(c => c.updateSelf(rawInterval.seconds))
					}
				} else if (timeData.timeAcceleration < 1.0f) { // slow-motion, update a partial interval
					if (timeData.timeAcceleration > 0.0f) { // if it's zero we're paused, do nothing
						localComponents.foreach(c => c.updateSelf((rawInterval * timeData.timeAcceleration).seconds))
					}
				} else {
					timeData.timeWarp match {
						case Some(warp) => // we're time warping, so we're doing a single update covering a larger increment of
							// time. It's up to individual components to deal with this gracefully, if necessary
							localComponents.foreach(c => c.updateSelf((rawInterval * warp).seconds))
						case None => // simple case, no time warp, no acceleration
							localComponents.foreach(c => c.updateSelf(rawInterval.seconds))
					}
				}

			}
		}
	}
}
object GameEngine {
	class TimeComponent(engine:GameEngine,world : World) extends GameComponent(engine,world) {
		override protected def update(dt: UnitOfTime): Unit = {
			world.timeData.time += dt
		}
	}
}



