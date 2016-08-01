package arx.control

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import arx.engine.control.event.Event.TEventUser
import arx.engine.world.World
import arx.gui2.Widget

import scalaxy.loops._

abstract class ControlMode(val controlEngine : ControlEngine) extends TEventUser {
	var name = this.getClass.getSimpleName
	def activate()
	def deactivate()

	def update(dt: UnitOfTime)

	def popMode(): Unit = {
		controlEngine.popMode(this)
	}

	def pushMode(newMode : ControlMode): Unit = {
		controlEngine.pushMode(newMode)
	}

	def isTopLevelMode = controlEngine.modeStack.headOption.contains(this)
	def addWidget(widget : Widget): Widget = {
		controlEngine.windowingSystem.addTopLevelWidget(widget)
		widget
	}

	def removeWidget(widget : Widget) : Widget = {
		controlEngine.windowingSystem.removeTopLevelWidget(widget)
		widget
	}

	def createWidget() : Widget = {
		val widget = new Widget(None)
		widget.windowingSystem = controlEngine.windowingSystem
		widget
	}

	def world : World = controlEngine.world

	override def toString: String = name
}
