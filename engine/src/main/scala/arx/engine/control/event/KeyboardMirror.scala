package arx.engine.control.event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/12/15
 * Time: 10:39 AM
 */

import org.lwjgl.glfw.GLFW._

import scala.collection.mutable

object KeyboardMirror {

	case class KeyState(var isDown: Boolean)

	val dummyKeyState = KeyState(isDown = false)

	val keyStates = new mutable.HashMap[Int, KeyState]

	def setKeyDown(k: Int, isDown: Boolean): Unit = {
		val ks = keyStates.getOrElseUpdate(k, KeyState(isDown))
		ks.isDown = isDown
	}

	def isKeyDown(k: Int) = {
		keyStates.getOrElse(k, dummyKeyState).isDown
	}

	def activeModifiers = KeyModifiers(ctrlActive, shiftActive, altActive)

	protected def ctrlActive = isKeyDown(GLFW_KEY_LEFT_CONTROL) || isKeyDown(GLFW_KEY_RIGHT_CONTROL) || isKeyDown(GLFW_KEY_LEFT_SUPER) || isKeyDown(GLFW_KEY_RIGHT_SUPER)

	protected def shiftActive = isKeyDown(GLFW_KEY_LEFT_SHIFT) || isKeyDown(GLFW_KEY_RIGHT_SHIFT)

	protected def altActive = isKeyDown(GLFW_KEY_LEFT_ALT) || isKeyDown(GLFW_KEY_RIGHT_ALT)

	def isActive (modifiers : KeyModifiers) = {
		modifiers.ctrl == ctrlActive &&
		modifiers.shift == shiftActive &&
		modifiers.alt == altActive
	}
}
