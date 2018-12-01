package arx.engine.advanced.lenginecomponents

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/17/18
  * Time: 8:18 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.engine.advanced.lenginepieces.LGameEngine
import arx.engine.event.EventBusListener
import arx.engine.game.GameEngine
import arx.engine.lworld.LWorld
import arx.engine.traits.EngineComponent

abstract class LGameComponent(val gameEngine: LGameEngine) extends EngineComponent[LWorld](gameEngine.world, gameEngine) {
	val gameEvents: EventBusListener = gameEngine.eventBus.createListener()
	val eventBus = gameEngine.eventBus

	override def listeners: List[EventBusListener] = List(gameEvents)
}
