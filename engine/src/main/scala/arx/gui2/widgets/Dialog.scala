package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/13/14
 * Time: 8:39 AM
 */

import arx.core.representation.ConfigValue
import arx.core.vec.Vec4f
import arx.engine.control.event.Event.UIEvent
import arx.graphics.Image
import arx.gui2.Widget
import arx.gui2.arrangement.Orientation
import arx.core.ImplicitModdable._
import arx.core.Moddable
import arx.core.vec.Cardinals._
import arx.engine.control.event.Event.KeyPressEvent
import arx.gui2.events.TextInputEnter
import arx.resource.ResourceManager.image
import org.lwjgl.glfw.GLFW

class Dialog(parentis:Widget) extends Widget(parentis) {
	val promptText = new TextDisplayWidget("OK?",this)
	val doneButton = TextButton("ok",this)
	val cancelButton = TextButton("cancel",this)

	windowingSystem.addToFocusStack(this)
	doneButton.onClick { finish(success = true) }
	cancelButton.onClick { finish(success = false) }

	width = 40.0f
	height = 25.0f
	backgroundImage = image("ui/fancyBackgroundWhite_ns.png")
	pixelScale = 1
	this.centerVertically()
	this.centerHorizontally()
	z = 15.0f

	makeModal()

	promptText.width = clientWidth _
	promptText.height = 10.0f
	promptText.backgroundImage = Image.Sentinel
	promptText.textAlignment = Center

	doneButton.dockToInteriorRight(1.0f)
	cancelButton.x = 1.0f

	for (b <- List(cancelButton,doneButton)) {
		b.height = 3.0f
		b.width = 10.0f
		b.dockToInteriorBottom(1.0f)
		b.backgroundImage = image("ui/fancyBackgroundWhite_ns.png")
		b.pixelScale = 1
		b.textAlignment = Center
		b.backgroundColor = Vec4f(0.7f,0.7f,0.7f,1.0f)
	}

	def finish (success : Boolean): Unit = {
		windowingSystem.popFocusStack()
		this.close()
		if (success) {
			onDialogSuccess()
		} else {
			onDialogFailure()
		}
		fireEvent(DialogFinishedEvent(success))
	}

	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		super.setFromSML (sml, overwrite)

		if (sml.doneText.nonEmptyValue) {
			doneButton.text = sml.doneText.str
		}
		if (sml.cancelText.nonEmptyValue) {
			cancelButton.text = sml.cancelText.str
		}
		if (sml.doneButton.nonEmptyValue) {
			doneButton.setFromSML(sml.doneButton,overwrite)
		}
		if (sml.cancelButton.nonEmptyValue) {
			cancelButton.setFromSML(sml.cancelButton,overwrite)
		}
		promptText.text = sml.promptText.strOrElse(promptText.text)
	}

	onEvent {
		case KeyPressEvent(key,modifier,_) if key == GLFW.GLFW_KEY_ESCAPE => finish(success = false)
		case KeyPressEvent(key,modifier,_) if key == GLFW.GLFW_KEY_ENTER => finish(success = true)
		case TextInputEnter(text) => finish(success = true)
	}

	var onDialogSuccess : () => Unit = () => {}
	var onDialogFailure : () => Unit = () => {}

	override protected def SMLTypeIdentifier: String = "dialog"
}

object Dialog {
	def apply (promptText : String, okText : String, cancelText : String, parentis : Widget) = {
		val d = new Dialog(parentis)
		d.promptText.text = promptText
		d.doneButton.text = okText
		d.cancelButton.text = cancelText
		d
	}
}

class InputDialog(parentis : Widget) extends Dialog(parentis) {
	this.height += 3.0f
	val input = new TextInput(this)
	input.dockBelow(this.promptText)
	input.centerHorizontally()
	input.width = this.clientWidth - 4.0f
	input.height = 4.0f
	input.backgroundImage = image("ui/singlePixelBorderWhite_ne.png")
	input.takeFocus()

//	var onInputSuccess : (String) => Unit = () => {}

	def inputValue = input.currentText
}

object InputDialog {
	def apply (promptText : String, okText : String, cancelText : String, parentis : Widget) = {
		val d = new InputDialog(parentis)
		d.promptText.text = promptText
		d.doneButton.text = okText
		d.cancelButton.text = cancelText

		d
	}
}

case class DialogFinishedEvent (agreed : Boolean) extends UIEvent {

}

class ColorPickerDialog(parentis : Widget) extends Dialog(parentis) {
	this.height += 8.0f
	val input = new ColorPickerWidget(this,Orientation.Horizontal)
	input.dockBelow(this.promptText)
	input.centerHorizontally()
	input.backgroundImage = Image.Sentinel
//	input.width = this.clientWidth - 4.0f
//	input.height = 4.0f
//	input.backgroundImage = image("ui/singlePixelBorderWhite_ne.png")
//	input.takeFocus()

	def inputValue = input.activeColorHSBA
}

object ColorPickerDialog {
	def apply (startColor : HSBA, promptText : String, okText : String, cancelText : String, parentis : Widget) = {
		val d = new ColorPickerDialog(parentis)
		d.promptText.text = promptText
		d.doneButton.text = okText
		d.cancelButton.text = cancelText
		d.input.setColorTo(startColor)

		d
	}
}