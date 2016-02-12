package arx.graphics.pov

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:33 PM
 */

import arx.Prelude._
import arx.core.vec.{ReadVec3f, Vec3f}
import arx.engine.control.event.Event.KeyModifiers
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.control.event.Event.Keymap
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

class TopDownCamera(zDist : Float) extends EyeCamera(Vec3f(0,0,zDist), Vec3f(0,0,-1), Vec3f(0,1,0)) {
	override def keymapNamespace: String = TopDownCamera.namespace

	var proportionalMoveSpeed = false

	override def effectiveMoveSpeed: ReadVec3f = if (proportionalMoveSpeed) {
		moveSpeed * (eye.z / 100.0f)
	} else {
		super.effectiveMoveSpeed
	}
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
