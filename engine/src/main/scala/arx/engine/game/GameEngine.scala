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
import arx.engine.event.EventBus
import arx.engine.game.components.GameComponent
import arx.engine.world.World
import arx.Prelude._
import arx.application.Noto
import arx.engine.entity.TGameEntity
import arx.engine.game.events.EntityAddedEvent
import arx.engine.game.events.EntityRemovedEvent

import scalaxy.loops._

class GameEngine(val world: World, val eventBus : EventBus) extends EnginePiece[GameComponent] {
	val self = this

	parallelism = 4
	def graphicsComponents = components

//	addComponent[GameEngine.TimeComponent]

	def advanceTime(deltaSeconds : Float): Unit = {
		val dt = deltaSeconds.seconds
		world.timeData.time += dt * world.timeData.timeAcceleration
	}


	override def initialize(serial: Boolean): Unit = {
		super.initialize(serial)

		world.createEntityQuery {
			case e : TGameEntity => e
		}.onAddition(e => eventBus.fireEvent(EntityAddedEvent(e)), fireOnExistingResults = true)
		.onRemoval(e => eventBus.fireEvent(EntityRemovedEvent(e)))
	}

	override def update(deltaSeconds: Float): Unit = {
		super.update(deltaSeconds)
		advanceTime(deltaSeconds)
	}

	override def updateSerial(deltaSeconds: Float, nSteps: Int): Unit = {
		super.updateSerial(deltaSeconds, nSteps)
		advanceTime(deltaSeconds)
	}

	override def currentTime(): UnitOfTime = world.timeData.time

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try {c.getConstructor(classOf[GameEngine]) != null} catch {case e: Exception => false}) match {
			case Some(clazz) =>
				val constructor = clazz.getConstructor(classOf[GameEngine])
				constructor.newInstance(this).asInstanceOf[GameComponent]
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}
}
//
//
//object GameEngine {
//	class TimeComponent(engine:GameEngine) extends GameComponent(engine) {
//		override protected def update(dt: UnitOfTime): Unit = {
//			world.timeData.time += dt
//		}
//	}
//}



