package arx.engine.control.components.windowing.widgets

import arx.core.Moddable
import arx.core.vec.{Vec2T, Vec2i}
import arx.engine.control.components.windowing.Widget
import arx.engine.control.event.Event.{CharEnteredEvent, KeyPressEvent}
import arx.graphics.helpers.RichText
import arx.Prelude._
import org.lwjgl.glfw.GLFW

class TextInputWidget(parentis : Widget) extends Widget(parentis) {
	var cursorPosition : Option[Int] = None
	var text = ""

	drawing.backgroundImage = Some("ui/minimalistBorderWhite_ne.png")
//	drawing.interiorPadding = Vec2i(10,10)

	val textDisplay = new TextDisplayWidget(this)
	textDisplay.dimensions = Vec2T(DimensionExpression.MatchParent, DimensionExpression.Intrinsic)
	textDisplay.text = Moddable(() => RichText(text))


	onEvent {
		case CharEnteredEvent(str) =>
			text += str
		case KeyPressEvent(key, modifiers, _) =>
			key pmatch {
				case GLFW.GLFW_KEY_BACKSPACE =>
					text = text.dropRight(1)
				case GLFW.GLFW_KEY_ENTER =>
					text = text + "\n"
			}

	}
}
