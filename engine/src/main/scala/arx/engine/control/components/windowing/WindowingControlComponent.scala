package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.core.units.UnitOfTime
import arx.engine.advanced.lenginecomponents.LControlComponent
import arx.engine.advanced.lenginepieces.LControlEngine
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent

class WindowingControlComponent(controlEngine : ControlEngine) extends ControlComponent(controlEngine) {
	val windowingSystem = new WindowingSystem(controlEngine.controlWorld, controlEngine.graphicsWorld, controlEngine.eventBus)


	override protected def initialize(): Unit = {
//		graphics[WindowingGraphicsData].desktop = control[WindowingData].desktop
	}


	override protected def updateSelf(dt: UnitOfTime): Unit = {
		windowingSystem.update()
	}
}


class LWindowingControlComponent(controlEngine : LControlEngine) extends LControlComponent(controlEngine) {
	val windowingSystem = new WindowingSystem(controlEngine.controlWorld, controlEngine.graphicsWorld, controlEngine.eventBus)


	override protected def initialize(): Unit = {
		//		graphics[WindowingGraphicsData].desktop = control[WindowingData].desktop
	}


	override protected def updateSelf(dt: UnitOfTime): Unit = {
		windowingSystem.update()
	}
}