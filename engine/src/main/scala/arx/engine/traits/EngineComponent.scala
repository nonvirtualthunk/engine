package arx.engine.traits

/**
  * TODO: Add javadoc
  */

import java.util.concurrent.atomic.AtomicBoolean

import arx.Prelude._
import arx.core.TDependable
import arx.core.units.UnitOfTime
import arx.engine.EnginePiece
import arx.engine.data.TWorldAuxData
import arx.engine.event.EventBusListener
import arx.engine.world.World

import scalaxy.loops._

abstract class EngineComponent(val world : World, enginePiece : EnginePiece[_]) extends TDependable {
	val updateInProgress = new AtomicBoolean(false)
	var lastUpdated = 0.seconds
	var initialized = false
	var updateInterval = (1/60.0).seconds

	def listeners : List[EventBusListener]

	def update(dt : UnitOfTime): Unit = {
		if (!initialized) {
			initialize()
			initialized = true
		}
		// process all the asynchronously queued events
		listeners.foreach(l => l.process())

		updateSelf(dt)
		lastUpdated = curTime()
	}

	protected def updateSelf(dt : UnitOfTime): Unit = {

	}

	protected def initialize(): Unit = {

	}

	enginePiece.registerComponent(this)
}
