package arx.graphics.pov

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:33 PM
 */

import arx.Prelude._
import arx.core.vec.Vec3f
import arx.engine.control.event.KeyModifiers
import arx.engine.control.event.KeyPressEvent
import arx.engine.control.event.Keymap
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

class TopDownCamera extends EyeCamera(Vec3f(0,0,1), Vec3f(0,0,-1), Vec3f(0,1,0)) {
	override def keymapNamespace: String = TopDownCamera.namespace
}

object TopDownCamera {
	import EyeCamera._
	val namespace = "TopDownCamera"

	Keymap.register(namespace, PanLeft, GLFW.GLFW_KEY_LEFT, KeyModifiers.Ctrl)
	Keymap.register(namespace, PanRight, GLFW.GLFW_KEY_RIGHT, KeyModifiers.Ctrl)

	Keymap.register(namespace, MoveLeft, GLFW.GLFW_KEY_LEFT)
	Keymap.register(namespace, MoveRight, GLFW.GLFW_KEY_RIGHT)
	Keymap.register(namespace, MoveForward, GLFW.GLFW_KEY_X)
	Keymap.register(namespace, MoveBack, GLFW.GLFW_KEY_Z)
	Keymap.register(namespace, MoveUp, GLFW.GLFW_KEY_UP)
	Keymap.register(namespace, MoveDown, GLFW.GLFW_KEY_DOWN)
}
