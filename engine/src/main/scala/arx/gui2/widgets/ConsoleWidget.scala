package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/23/13
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 */

import java.io.StringWriter
import java.util.concurrent.Callable

import arx.Prelude._
import arx.application.Noto
import arx.core.async.Executor
import arx.gui2.Widget
import arx.core.ImplicitModdable._
import arx.core.Moddable
import arx.core.repl.ArxInterpreter
import arx.core.vec.Cardinals._
import arx.engine.control.event.Event.KeyPressEvent
import arx.gui2.events.TextInputEnter

class ConsoleWidget(parentis:Widget) extends Widget(parentis) {
	val sw = new StringWriter()

	val _interpreter = Executor.submitAsync( new Callable[ArxInterpreter]{
		def call(): ArxInterpreter = {
			val interpreter = new ArxInterpreter(sw)
			_onLoad.foreach( f => f(interpreter) )
			interpreter
		}
	} )

	def interpreter = _interpreter.get()

	def onLoad ( func : (ArxInterpreter) => Unit ) {
		_onLoad ::= func
	}
	protected var _onLoad = List[(ArxInterpreter) => Unit]()

	width = 50.0f
	height = 85.0f

	def consoleOutput = sw.toString
	val displayWidget = new Widget(this)
	displayWidget.width = () => this.clientWidth.resolve() - 2.0f
	displayWidget.height = () => this.clientHeight - 5.0f

	val outputText = new TextDisplayWidget(() => consoleOutput,displayWidget)
	outputText.fontSize = 2.5f
	outputText.width = () => displayWidget.clientWidth
	outputText.height = () => displayWidget.clientHeight
//	outputText.width = () => displayWidget.clientWidth.resolve()
//	outputText.height = () => outputText.textRenderer.currentLayout.dimensions.y

	val inputWidget = new TextEditorWidget(this)
	inputWidget.singleLine = true
	inputWidget.width = () => this.clientWidth.resolve()
	inputWidget.height = 5.0f
	inputWidget.fontSize = 3.0f
	inputWidget.y = () => displayWidget.y + displayWidget.height

	var inputBuffer = List[String]()
	var inputBackIndex = 0

	inputWidget.onEvent {
		case TextInputEnter(str) => {
			try{
				sw.getBuffer.setLength(0)
				inputWidget.clear()
				if ( str.startsWith("smartImport") ) {
					interpreter.smartImport( str.substring( "smartImport".length + 1 ).trim )
				} else if ( str.startsWith("`") && ! str.contains(".") ) {
					interpreter.smartImport( str.substring(1).trim )
				} else if ( str.startsWith("import ") ) {
					interpreter.addImport( str.substring( "import".length + 1 ).trim )
				} else if ( str.startsWith("reify ") ) {
					interpreter.reify( str.substring("reify ".length).trim )
				} else {
					interpreter.interpret(str)
//					val heightDiff = outputText.textHeight - displayWidget.clientHeight
//					displayWidget.scrollPosition.y = -heightDiff
				}
				if ( inputBackIndex == 0 ) {
					if ( inputBuffer.isEmpty || inputBuffer.head != str ) {
						if ( ! str.isEmpty ) {
							inputBuffer ::= str
						}
					}
				} else {
					inputBackIndex = 0
				}
			} catch {
				case e : Exception => Noto.error("Exception encountered in interpreter, brushing it under the rug")
			}
		}
//		case TextInputWidget.TextInputUp(str) => {
//			if ( inputBackIndex < inputBuffer.size -1 ) {
//				if ( inputBackIndex == 0 ) {
//					if ( ! str.isEmpty ) {
//						inputBuffer ::= str
//						inputBackIndex += 1
//
//					}
//				} else {
//					inputBackIndex += 1
//				}
//				inputWidget.currentText = inputBuffer(inputBackIndex)
//				inputWidget.cursorIndex = inputWidget.currentText.size
//			}
//		}
//		case TextInputWidget.TextInputDown(str) => {
//			if ( inputBackIndex > 0 ) {
//				inputBackIndex -= 1
//				inputWidget.currentText = inputBuffer(inputBackIndex)
//				inputWidget.cursorIndex = inputWidget.currentText.size
//			}
//		}

			// TODO: Mode action event
//		case KeyPressEvent(keyId,_) if keyId == Keyboard.KEY_ESCAPE => fireEvent( ModeActionEvent(ModeConstants.ExitMode) )
	}

	var lastTextHeight = 0.0f
	override def updateLogic(f: Float) {
//		val currentTextHeight = outputText.textHeight
//		if ( currentTextHeight != lastTextHeight ) {
//			lastTextHeight = currentTextHeight
//			val heightDiff = currentTextHeight - displayWidget.clientHeight
//			if ( heightDiff > 0.0f ) {
//				displayWidget.scrollPosition.y = -heightDiff
//			}
//		}
	}

}
