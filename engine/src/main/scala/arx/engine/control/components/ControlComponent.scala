package arx.engine.control.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.engine.control.ControlEngine
import arx.engine.control.data.ControlModes
import arx.engine.control.data.TControlData
import arx.engine.event.EventBusListener
import arx.engine.graphics.data.TGraphicsData
import arx.engine.traits.EngineComponent

import scalaxy.loops._

abstract class ControlComponent(controlEngine : ControlEngine) extends EngineComponent(controlEngine.world, controlEngine) {

	val gameEvents = controlEngine.gameEventBus.createListener()
	val graphicsEvents = controlEngine.graphicsEventBus.createListener()
	val controlEvents = controlEngine.eventBus.createListener()


	override def listeners: List[EventBusListener] = List(gameEvents,graphicsEvents,controlEvents)

	def graphics[T <: TGraphicsData : Manifest] = controlEngine.graphicsWorld.aux[T]
	def control[T <: TControlData : Manifest] = controlEngine.controlWorld.aux[T]
}

abstract class ControlMode(controlEngine : ControlEngine) extends ControlComponent(controlEngine) {
	def activate()
	def deactivate()

	def pushMode[T <: ControlMode : Manifest] = {
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