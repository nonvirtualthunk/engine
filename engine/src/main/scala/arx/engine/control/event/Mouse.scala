package arx.engine.control.event.Event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 8:01 AM
 */

import java.util.concurrent.atomic.AtomicReference

import arx.Prelude._
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.vec.ReadVec2f
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

object Mouse {
	def setVisible(visible: Boolean) = {
		if (visible != isVisible) {
			isVisible = visible
			if (!visible) {
				GLFW.glfwSetInputMode(windowRef, GLFW.GLFW_CURSOR, GLFW.GLFW_CURSOR_HIDDEN)
			} else {
				GLFW.glfwSetInputMode(windowRef, GLFW.GLFW_CURSOR, GLFW.GLFW_CURSOR_NORMAL)
			}
		}
	}

	var _currentPosition = new AtomicReference(ReadVec2f(0.0f,0.0f))
	var _previousPosition = new AtomicReference(ReadVec2f(0.0f,0.0f))
	var _buttonDown = Map(MouseButton.Left -> false, MouseButton.Right -> false, MouseButton.Middle -> false, MouseButton.Other -> false)
	var isVisible = true
	var windowRef = 0L

	def currentPosition = _currentPosition.get()
	def currentPosition_=(p : ReadVec2f) { _currentPosition.set(p) }

	def previousPosition = _previousPosition.get()
	def previousPosition_=(p : ReadVec2f) { _previousPosition.set(p) }

	def buttonDown : Map[MouseButton,Boolean] = synchronized { _buttonDown }
	def buttonDown_=(m : Map[MouseButton, Boolean]) = synchronized { _buttonDown = m }

	def updatePosition(pos : ReadVec2f): Unit = {
		_previousPosition.set(currentPosition)
		currentPosition = pos
	}
}

class MouseButton(name : String) extends ArxEnum(name) {}

object MouseButton extends ArxEnumObject[MouseButton] {
	val Left = MouseButton("Left")
	val Right = MouseButton("Right")
	val Middle = MouseButton("Middle")
	val Other = MouseButton("Other")
}
