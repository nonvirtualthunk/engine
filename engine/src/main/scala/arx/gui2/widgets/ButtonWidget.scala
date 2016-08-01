package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/13/13
 * Time: 12:04 PM
 */

import arx.Prelude._
import arx.core.ImplicitModdable._
import arx.core.Moddable
import arx.core.vec.Cardinals._
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.gui2.Widget

trait Button extends Widget {
	def isPressed = {
		windowingSystem.lastWidgetUnderMouse match {
			case Some(w) => if ( this.contains(w) ) {
				windowingSystem.currentPressedWidget match {
					case Some(pw) => if ( this.contains(pw) ) {
						true
					} else { false }
					case None => false
				}
			} else { false }
			case None => false
		}
	}


	protected def effectiveBackgroundColor (c : ReadVec4f) = {
		if ( isPressed ) { Vec4f(c.r*0.65f,c.g*0.65f,c.b*0.65f,c.a) } else { c }
	}
	override def backgroundColor = effectiveBackgroundColor(super.backgroundColor)
}

class TextButton(parentis:Widget) extends TextDisplayWidget(parentis) with Button {
	textAlignment = Center

	override protected[gui2] def SMLTypeIdentifier: String = "text button"
}
object TextButton {
	def apply ( text : Moddable[String], p : Widget ) = {
		val tw = new TextButton(p)
		tw.text = text
		tw
	}
}

class ImageButton(parentis:Widget) extends ImageDisplayWidget(parentis) with Button {
	anchorTo = Center
	override protected[gui2] def SMLTypeIdentifier: String = "image button"
}

object ButtonWidget {
	def apply ( text : Moddable[String] , parentis : Widget ) = {
		new TextDisplayWidget(text,parentis) with Button
	}
}
