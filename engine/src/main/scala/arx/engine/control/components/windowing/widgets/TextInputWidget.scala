package arx.engine.control.components.windowing.widgets

import arx.core.Moddable
import arx.core.vec.{ReadVec4f, Vec2T, Vec2i, Vec4f}
import arx.engine.control.components.windowing.Widget
import arx.engine.control.event.Event.{CharEnteredEvent, KeyPressEvent, TextInputChanged, TextInputEnter}
import arx.graphics.helpers.{Color, RichText, TextSection}
import arx.Prelude._
import org.lwjgl.glfw.GLFW

//class TextInputWidget(parentis : Widget) extends Widget(parentis) {
//	var cursorPosition : Option[Int] = None
//	protected var _text = ""
//	def text = _text
//	var promptText = ""
//	var promptColor : Moddable[ReadVec4f] = Moddable(Vec4f(0.5f,0.5f,0.6f,1.0f))
//
//	var singleLine = true
//
//	drawing.backgroundImage = Some("ui/minimalistBorderWhite_ne.png")
//	drawing.interiorPadding = Vec2i(4,4)
//
//	val textDisplay = new TextDisplayWidget(this)
//	textDisplay.dimensions = Vec2T(DimensionExpression.Intrinsic, DimensionExpression.Intrinsic)
//	textDisplay.text = Moddable(() => richTextFromRaw(_text))
//
//	def richTextFromRaw(str : String) = RichText(str)
//
//	val promptTextDisplay = new TextDisplayWidget(this)
//	promptTextDisplay.dimensions = Vec2T(DimensionExpression.Intrinsic, DimensionExpression.Intrinsic)
//	promptTextDisplay.text = Moddable(() => RichText(TextSection(promptText, promptColor.resolve(), None) :: Nil))
//	promptTextDisplay.x = PositionExpression.Relative(textDisplay, 1)
//
//
//	def setText(newText : String, suppressEvent: Boolean = false): Unit = {
//		_text = newText
//		if (!suppressEvent) {
//			handleEvent(TextInputChanged(_text))
//		}
//	}
//
//	def setTextPrompt(newPrompt : String) : Unit = {
//		promptText = newPrompt
//	}
//
//	onEvent {
//		case CharEnteredEvent(str) =>
//			setText(text + str)
//		case KeyPressEvent(key, modifiers, _) =>
//			key pmatch {
//				case GLFW.GLFW_KEY_BACKSPACE =>
//					setText(text.dropRight(1))
//				case GLFW.GLFW_KEY_ENTER =>
//					if (singleLine) {
//						handleEvent(TextInputEnter(text))
//					} else {
//						setText(text + "\n")
//					}
//			}
//
//	}
//}
