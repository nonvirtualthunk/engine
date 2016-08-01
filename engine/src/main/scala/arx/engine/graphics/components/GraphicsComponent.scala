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
import arx.Prelude._

abstract class GraphicsComponent(val graphicsEngine : GraphicsEngine) extends TDependable with TUpdateable {
	var name = this.getClass.getSimpleName
	val world = graphicsEngine.world

	var drawPriority = 0
	var minimumUpdateInterval = 0.0166667.seconds
	var lastUpdated = System.nanoTime()
	var pov : Moddable[TCamera] = Moddable(() => graphicsEngine.pov)

	protected var initialized = false

	def draw ()


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
