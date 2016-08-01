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
import arx.engine.game.GameEngine
import arx.engine.world.World
import scalaxy.loops._

abstract class GameComponent(gameEngine: GameEngine) extends TDependable with TUpdateable {
	val world = gameEngine.world
	protected var initialized = false

	override def updateSelf(dt: UnitOfTime): Unit = {
		if (!initialized) {
			initialize()
			initialized = true
		}
		super.updateSelf(dt)
	}

	protected def initialize(): Unit = {

	}
}
