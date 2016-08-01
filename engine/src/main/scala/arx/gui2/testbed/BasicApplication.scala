package arx.gui2.testbed

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/26/14
 * Time: 8:13 AM
 */

import arx.core.vec.Vec4f
import arx.engine.advanced.Engine
import arx.engine.control.event.Event.Event
import arx.gui2.Widget
import arx.gui2.WindowingSystem2

class BasicApplication( mainWidget : => Widget) extends Engine {

	override def setUpEngine(): Unit = {
		clearColor = Vec4f.One

		val w = mainWidget
		windowingSystem.addTopLevelWidget(w)
		if (windowingSystem.focusedWidget.isEmpty) {
			windowingSystem.lastWidgetUnderMouse = Some(w)
			windowingSystem.giveFocusTo(w)
		}
	}
}