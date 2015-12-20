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
import arx.engine.game.GameEngine
import arx.engine.world.World
import scalaxy.loops._

abstract class GameComponent(gameEngine: GameEngine, world : World) extends TDependable with TUpdateable {

}
