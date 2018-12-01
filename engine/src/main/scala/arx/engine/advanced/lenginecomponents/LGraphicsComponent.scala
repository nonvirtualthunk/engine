package arx.engine.advanced.lenginecomponents

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:19 AM
  */

import arx.Prelude._
import arx.core.Moddable
import scalaxy.loops._
import arx.core.vec._
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.event.EventBusListener
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.data.{PovData, TGraphicsData}
import arx.engine.lworld.{LWorld, LWorldView}
import arx.engine.traits.EngineComponent
import arx.engine.world.World
import arx.graphics.pov.TCamera

abstract class LGraphicsComponent(val graphicsEngine : LGraphicsEngine) extends EngineComponent[LWorldView](graphicsEngine.view, graphicsEngine) {
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