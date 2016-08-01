package arx.gui2.testbed

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 1/6/13
 * Time: 1:55 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.Vec3f
import arx.gui2.Widget
import arx.gui2.widgets.ColorPickerWidget
import arx.gui2.widgets.HSBDisplayWidget
import arx.core.ImplicitModdable._
import arx.resource.ResourceManager

class HSBTestWidget extends Widget (None) {
	width = windowingSystem.dimensions.x _
	height = windowingSystem.dimensions.y _

	val sub2 = new HSBDisplayWidget(this,List(Vec3f(0.0f,0.5f,0.0f),Vec3f(1.0f,0.5f,0.0f),Vec3f(1.0f,0.5f,1.0f),Vec3f(0.0f,0.5f,1.0f)))
	sub2.x = 50.0f
	sub2.y = 10.0f
	sub2.width = 40.0f
	sub2.height = 40.0f

	val sub = new ColorPickerWidget(this)//(this,List(Vec3f(0.0f,0.5f,0.0f),Vec3f(1.0f,0.5f,0.0f),Vec3f(1.0f,0.5f,1.0f),Vec3f(0.0f,0.5f,1.0f)))
//	sub.width = 10.0f
//	sub.height = 10.0f
	sub.x = 5.0f
	sub.y = 5.0f

}


object HSBTestApplication extends BasicApplication(new HSBTestWidget) {

}

class TestWidget extends Widget(None) {
	width = 80.0f
	height = 80.0f
	x = 10.0f
	y = 10.0f
	backgroundImage = ResourceManager.image("ui/fancyBackground_ns.png")
	pixelScale = 3
}

object BasicTestApplication extends BasicApplication(new TestWidget) {

}