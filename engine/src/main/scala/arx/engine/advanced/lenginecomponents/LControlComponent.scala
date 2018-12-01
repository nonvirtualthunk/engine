package arx.engine.advanced.lenginecomponents

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/18/18
  * Time: 8:21 AM
  */

import arx.Prelude._
import arx.application.Noto
import scalaxy.loops._
import arx.core.vec._
import arx.engine.advanced.lenginepieces.LControlEngine
import arx.engine.control.ControlEngine
import arx.engine.control.components.{ControlComponent, TControlMode}
import arx.engine.control.data.{ControlModes, TControlData}
import arx.engine.event.EventBusListener
import arx.engine.graphics.data.TGraphicsData
import arx.engine.lworld.LWorld
import arx.engine.traits.EngineComponent

class LControlComponent(controlEngine : LControlEngine) extends EngineComponent[LWorld](controlEngine.world, controlEngine){

	val gameEvents = controlEngine.gameEventBus.createListener()
	val graphicsEvents = controlEngine.graphicsEventBus.createListener()
	val controlEvents = controlEngine.eventBus.createListener()


	override def listeners: List[EventBusListener] = List(gameEvents,graphicsEvents,controlEvents)

	def graphics[T <: TGraphicsData : Manifest] = controlEngine.graphicsWorld.aux[T]
	def control[T <: TControlData : Manifest] = controlEngine.controlWorld.aux[T]
}

abstract class LControlMode(controlEngine : ControlEngine) extends ControlComponent(controlEngine) with TControlMode {
	def pushMode[T <: LControlMode : Manifest] = {
		controlEngine.components.firstOfType[T] match {
			case Some(comp) =>
				val CM = control[ControlModes]
				CM.modeStack.headOption.foreach(m => m.deactivate())
				CM.modeStack = CM.modeStack.push(comp)
				comp.activate()
			case None =>
				Noto.error(s"Attempted to push a mode that is not present in the engine: ${manifest[T]}")
		}
	}
}