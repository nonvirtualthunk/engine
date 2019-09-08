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
	var origin: Option[AnyRef] = None

	def withOrigin(o : AnyRef) = {
		origin = Some(o)
		this
	}
}

class KeyEvent(var _key: Int,var _modifiers: KeyModifiers,var press: Boolean) extends UIEvent {
	var asciiChar = '\0'
	GLFW.GLFW_KEY_0
	def withAscii ( c : Char) : KeyEvent = { asciiChar = c;this }
}
case class KeyPressEvent(key: Int,modifiers: KeyModifiers, isRepeat : Boolean = false) extends KeyEvent(key,modifiers,true) {
	def copy() = KeyPressEvent(key,modifiers,isRepeat).withAscii(asciiChar)
}
case class KeyReleaseEvent(key: Int,modifiers: KeyModifiers) extends KeyEvent(key,modifiers,false) {
	def copy() = KeyReleaseEvent(key,modifiers).withAscii(asciiChar)
}
case class CharEnteredEvent(str : String) extends UIEvent
class MouseButtonEvent(_mouseButton: MouseButton,_mousePos: ReadVec2f,_modifiers: KeyModifiers,press : Boolean ) extends UIEvent
case class MousePressEvent(mouseButton: MouseButton,mousePos: ReadVec2f,modifiers: KeyModifiers) extends MouseButtonEvent(mouseButton,mousePos,modifiers,true)
case class MouseReleaseEvent(mouseButton: MouseButton,mousePos: ReadVec2f,modifiers: KeyModifiers) extends MouseButtonEvent(mouseButton,mousePos,modifiers,false)
case class ScrollEvent(delta: ReadVec2f,modifiers: KeyModifiers) extends UIEvent
case class MouseMoveEvent(mousePos: ReadVec2f,mouseDelta: ReadVec2f,modifiers: KeyModifiers) extends UIEvent
case class MouseDragEvent(mousePos: ReadVec2f,mouseDelta: ReadVec2f,mouseButtons: Set[MouseButton],modifiers: KeyModifiers) extends UIEvent


case class TextInputChanged ( newStringValue : String ) extends Event
case class TextInputCursorMoved ( newCursorPosition : Int ) extends Event
case class TextInputEnter ( curStringValue : String ) extends UIEvent
case class TextInputCancel ( cancelledValue : String ) extends UIEvent
case class TextInputUp ( curStringValue : String ) extends UIEvent
case class TextInputDown ( curStringValue : String ) extends UIEvent