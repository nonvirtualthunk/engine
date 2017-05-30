package arx.engine.game.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:17 PM
 */

import arx.Prelude._
import arx.core.TDependable
import arx.core.traits.TUpdateable
import arx.core.units.UnitOfTime
import arx.engine.event.EventBusListener
import arx.engine.game.GameEngine
import arx.engine.traits.EngineComponent
import arx.engine.world.World

import scalaxy.loops._

abstract class GameComponent(val gameEngine: GameEngine) extends EngineComponent(gameEngine.world, gameEngine) {
	val gameEvents: EventBusListener = gameEngine.eventBus.createListener()
	val eventBus = gameEngine.eventBus

	override def listeners: List[EventBusListener] = List(gameEvents)
}
