package arx.engine.graphics.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 12:07 PM
 */

import arx.core.Moddable
import arx.core.TDependable
import arx.core.traits.TUpdateable
import arx.core.units.UnitOfTime
import arx.engine.graphics.GraphicsEngine
import arx.engine.world.World
import arx.graphics.pov.TCamera

abstract class GraphicsComponent(graphicsEngine : GraphicsEngine, world : World) extends TDependable with TUpdateable {
	var drawPriority = 0
	var pov : Moddable[TCamera] = Moddable(() => graphicsEngine.pov)

	def update (dt : UnitOfTime)

	def draw ()
}
