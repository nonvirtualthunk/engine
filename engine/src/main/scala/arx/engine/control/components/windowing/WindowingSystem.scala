package arx.engine.control.components.windowing

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 11/3/18
  * Time: 6:59 AM
  */

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, ClipboardOwner, DataFlavor, StringSelection, Transferable}
import java.io.IOException

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfTime
import scalaxy.loops._
import arx.core.vec._
import arx.engine.control.components.windowing.events.{DropEvent, FocusGainedEvent, FocusLostEvent, RequestFocusEvent}
import arx.engine.control.components.windowing.widgets.data.EventHandlingData
import arx.engine.control.data.{ControlWorld, WindowingData}
import arx.engine.control.event.Event._
import arx.engine.event.EventBus
import arx.engine.graphics.data.{GraphicsWorld, WindowingGraphicsData}
import arx.graphics.GL

class WindowingSystem(controlWorld : ControlWorld, graphicsWorld : GraphicsWorld, controlBus : EventBus) {
	val WD = controlWorld.auxData[WindowingData]
	import WD._
	val WGD = graphicsWorld.auxData[WindowingGraphicsData]
	WGD.desktop = WD.desktop

	WD.desktop.onEvent {
		case RequestFocusEvent(widget) => giveFocusTo(widget)
	}


	controlBus.onEvent {
		case kpe : KeyPressEvent =>
			focusedWidget.exists(w => w.handleEvent(kpe.copy().withOrigin(w)))
		case kre : KeyReleaseEvent =>
			focusedWidget.exists(w => w.handleEvent(kre.copy().withOrigin(w)))
		case kee : CharEnteredEvent =>
			focusedWidget.exists(w => w.handleEvent(kee.copy().withOrigin(w)))
		case sre : ScrollEvent =>
			lastWidgetUnderMouse.exists(w => w.handleEvent(sre.copy().withOrigin(w)))
		case mme : MouseMoveEvent =>
			lastWidgetUnderMouse = widgetAtMousePosition(mme.mousePos)
			lastWidgetUnderMouse.exists(w => w.handleEvent(mme.copy().withOrigin(w)))
		case mde : MouseDragEvent =>
			lastWidgetUnderMouse = widgetAtMousePosition(mde.mousePos)
			currentPressedWidget.exists(w => {
				w.selfAndAncestors.find(_.dragAndDropRO.draggable) match {
					// if there's something to drag, start dragging it, we may drop it onto something later
					case Some(draggableWidget) =>
						draggingWidget = Some(draggableWidget)
						true
					// elsewise, treat it as a normal event like any other
					case None => w.handleEvent(mde.copy().withOrigin(w))
				}
			});
		case mpe : MousePressEvent =>
			widgetAtMousePosition(mpe.mousePos) match {
				case Some(w) =>
					var passOn = true
					if (modalWidgetStack.nonEmpty) {
						if (!modalWidgetStack.head.widget.selfAndAncestors.contains(w)) {
							passOn = false
							if (modalWidgetStack.head.closeOnClickElsewhere) {
								modalWidgetStack.head.widget.close()
							}
						}
					}
					if (passOn) {
						giveFocusTo(w)
						currentPressedWidget = Some(w)
						w.handleEvent(mpe.copy().withOrigin(w))
					}
				case None =>
					currentPressedWidget = None
					false
			}
		case mre : MouseReleaseEvent if mre.mouseButton == MouseButton.Left =>
			val ret = widgetAtMousePosition(mre.mousePos) match {
				case Some(droppedOn) if draggingWidget.nonEmpty =>
					droppedOn.selfAndAncestors.find(_.dragAndDropRO.droppable) match {
						case Some(dropTarget) => dropTarget.handleEvent(DropEvent(draggingWidget.get,dropTarget,Vec2f.Zero))
						case None => true
					}
				case Some(w) =>
					w.handleEvent(mre.copy().withOrigin(w))
				case None =>
					if (draggingWidget.nonEmpty) {
						draggingWidget = None
						true
					} else {
						false
					}
			}
			// nothing is dragged or pressed any longer
			draggingWidget = None
			currentPressedWidget = None
			ret
		case mreo : MouseReleaseEvent =>
			widgetAtMousePosition(mreo.mousePos).exists(w => w.handleEvent(mreo.copy().withOrigin(w)))
	}


	def update() : Unit = {
		desktop.synchronized {
			updateWidget(desktop)
		}
	}

	def updateWidget(w : Widget): Unit = {
		w.updateSelf()
		w.children.foreach(c => updateWidget(c))
	}

	def widgetAtMousePosition(pos : ReadVec2f) : Option[Widget] = {
		WGD.pov.unprojectAtZ(pos,0.0f,GL.maximumViewport) match {
			case Some(clickedPos) =>
				WD.desktop.selfAndChildren.toStream.reverse.find(w => {
					val apos = w.drawing.absolutePosition
					val adim = w.drawing.effectiveDimensions
					apos.x <= clickedPos.x && apos.y <= clickedPos.y &&
						apos.x + adim.x >= clickedPos.x && apos.y + adim.y >= clickedPos.y
				})
			case None =>
				Noto.info("click did not intersect plane?")
				None
		}
	}

	def widgets = WD.desktop.selfAndAncestors

	def giveFocusTo(target : Widget): Unit = {
		for (newFocus <- target.selfAndAncestors.find(w => w.eventHandlingRO.acceptsFocus)) {
			focusedWidget.foreach(w => {
				w.handleEvent(FocusLostEvent(w))
				w[EventHandlingData].hasFocus = false
			})
			focusedWidget = Some(newFocus)
			newFocus.handleEvent(FocusGainedEvent(newFocus))
			newFocus[EventHandlingData].hasFocus = true
		}
	}
}

object WindowingSystem extends ClipboardOwner {
	def clipboardText : Option[String] = {
		val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
		try {
			val contents = clipboard.getContents(this)
			if ( contents.getTransferDataFlavors.contains( DataFlavor.stringFlavor ) ) {
				contents.getTransferData( DataFlavor.stringFlavor ) match {
					case string : String => {
						return Some(string)
					}
					case other => {
						Noto.warn(f"Despite requesting a string flavored clipboard, we got $other instead")
					}
				}
			}
		} catch {
			case ise : IllegalStateException => Noto.info("System clipboard is a bit throw-happy")
			case ioe : IOException => Noto.info("System clipboard got mad because its state changed, or something, string flavor not available")
		}
		None
	}

	def copyTextToClipboard ( str : String ) {
		val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
		try {
			clipboard.setContents(new StringSelection(str),this)
		} catch {
			case ise : IllegalStateException => Noto.info("System clipboard is a bit throw-happy")
			case ioe : IOException => Noto.info("System clipboard got mad because its state changed, or something, string flavor not available")
		}
	}

	override def lostOwnership(clipboard: Clipboard, contents: Transferable): Unit = {}
}