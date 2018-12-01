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
import arx.engine.event.EventBusListener
import arx.engine.graphics.data.PovData
import arx.engine.graphics.data.TGraphicsData
import arx.engine.traits.EngineComponent

abstract class GraphicsComponent(val graphicsEngine : GraphicsEngine) extends EngineComponent[World](graphicsEngine.world, graphicsEngine) {
	var name = this.getClass.getSimpleName

	val gameEvents = graphicsEngine.gameEventBus.createListener()
	val graphicsEvents = graphicsEngine.eventBus.createListener()


	override def listeners: List[EventBusListener] = List(gameEvents, graphicsEvents)

	var drawOrder = DrawPriority.Standard
	var pov : Moddable[TCamera] = Moddable(() => graphicsEngine.graphicsWorld[PovData].pov)

	def draw ()

	def graphics[T <: TGraphicsData : Manifest] = graphicsEngine.graphicsWorld.aux[T]
}


object GraphicsComponent {

}

object DrawPriority {
	val Early = -100
	val Standard = 0
	val Late = 100
	val Final = 200
}