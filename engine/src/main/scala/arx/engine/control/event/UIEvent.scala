package arx.engine.control.event.Event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/16/15
 * Time: 8:18 AM
 */

import arx.core.vec.ReadVec2f
import arx.core.vec.Vec3f
import org.lwjgl.glfw.GLFW

class UIEvent extends Event {

}

class KeyEvent(var _key: Int,var _modifiers: KeyModifiers,var press: Boolean) extends UIEvent {
	var asciiChar = '\0'
	GLFW.GLFW_KEY_0
	def withAscii ( c : Char) : KeyEvent = { asciiChar = c;this }
}
case class KeyPressEvent(key: Int,modifiers: KeyModifiers, isRepeat : Boolean = false) extends KeyEvent(key,modifiers,true)
case class KeyReleaseEvent(key: Int,modifiers: KeyModifiers) extends KeyEvent(key,modifiers,false)
case class CharEnteredEvent(str : String) extends UIEvent
class MouseButtonEvent(_mouseButton: MouseButton,_mousePos: ReadVec2f,_modifiers: KeyModifiers,press : Boolean ) extends UIEvent
case class MousePressEvent(mouseButton: MouseButton,mousePos: ReadVec2f,modifiers: KeyModifiers) extends MouseButtonEvent(mouseButton,mousePos,modifiers,true)
case class MouseReleaseEvent(mouseButton: MouseButton,mousePos: ReadVec2f,modifiers: KeyModifiers) extends MouseButtonEvent(mouseButton,mousePos,modifiers,false)
case class ScrollEvent(delta: ReadVec2f,modifiers: KeyModifiers) extends UIEvent
case class MouseMoveEvent(mousePos: ReadVec2f,mouseDelta: ReadVec2f,modifiers: KeyModifiers) extends UIEvent
case class MouseDragEvent(mousePos: ReadVec2f,mouseDelta: ReadVec2f,mouseButtons: Set[MouseButton],modifiers: KeyModifiers) extends UIEvent