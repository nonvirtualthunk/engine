package arx.engine.control.components

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.engine.advanced.lenginecomponents.LControlComponent
import arx.engine.advanced.lenginepieces.LControlEngine
import arx.engine.control.ControlEngine
import arx.engine.control.data.{ControlModes, LControlModes, TControlData}
import arx.engine.event.EventBusListener
import arx.engine.graphics.data.TGraphicsData
import arx.engine.traits.EngineComponent
import arx.engine.world.World
import scalaxy.loops._

abstract class ControlComponent(controlEngine : ControlEngine) extends EngineComponent[World](controlEngine.world, controlEngine) {

	val gameEvents = controlEngine.gameEventBus.createListener()
	val graphicsEvents = controlEngine.graphicsEventBus.createListener()
	val controlEvents = controlEngine.eventBus.createListener()


	override def listeners: List[EventBusListener] = List(gameEvents,graphicsEvents,controlEvents)

	def graphics[T <: TGraphicsData : Manifest] = controlEngine.graphicsWorld.aux[T]
	def control[T <: TControlData : Manifest] = controlEngine.controlWorld.aux[T]
}


trait TControlMode extends ControlComponent {
	final def activate(): Unit = {
		activateSelf()
		listeners.foreach(l => l.active = true)
	}
	def activateSelf()
	final def deactivate(): Unit = {
		deactivateSelf()
		listeners.foreach(l => l.active = false)
	}
	def deactivateSelf()
}

abstract class ControlModeComponent(controlEngine : ControlEngine) extends ControlComponent(controlEngine) with TControlMode {
	listeners.foreach(l => l.active = false)

	def pushMode[T <: ControlModeComponent : Manifest] = {
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

	def popMode(): Unit = {
		val CM = control[ControlModes]
		CM.modeStack.headOption match {
			case Some(top) =>
				top.deactivate()
				CM.modeStack = CM.modeStack.pop
				CM.modeStack.headOption.foreach(m => m.activate())
			case None =>
				Noto.error("Attempted to pop empty mode stack")
		}
	}
}

trait TLControlMode extends LControlComponent {
	final def activate(): Unit = {
		activateSelf()
		listeners.foreach(l => l.active = true)
	}
	def activateSelf()
	final def deactivate(): Unit = {
		deactivateSelf()
		listeners.foreach(l => l.active = false)
	}
	def deactivateSelf()
}


abstract class LControlModeComponent(val controlEngine : LControlEngine) extends LControlComponent(controlEngine) with TLControlMode {

	listeners.foreach(l => l.active = false)

	def pushMode[T <: LControlModeComponent : Manifest] = {
		controlEngine.components.firstOfType[T] match {
			case Some(comp) =>
				val CM = control[LControlModes]
				CM.modeStack.headOption.foreach(m => m.deactivate())
				CM.modeStack = CM.modeStack.push(comp)
				comp.activate()
			case None =>
				Noto.error(s"Attempted to push a mode that is not present in the engine: ${manifest[T]}")
		}
	}

	def popMode(): Unit = {
		val CM = control[LControlModes]
		CM.modeStack.headOption match {
			case Some(top) =>
				top.deactivate()
				CM.modeStack = CM.modeStack.pop
				CM.modeStack.headOption.foreach(m => m.activate())
			case None =>
				Noto.error("Attempted to pop empty mode stack")
		}
	}
}