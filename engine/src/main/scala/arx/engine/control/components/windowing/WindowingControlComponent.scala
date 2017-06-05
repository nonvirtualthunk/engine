package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.application.Noto
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.engine.control.ControlEngine
import arx.engine.control.components.ControlComponent
import arx.engine.control.components.windowing.events.DropEvent
import arx.engine.control.components.windowing.events.FocusGainedEvent
import arx.engine.control.components.windowing.events.FocusLostEvent
import arx.engine.control.data.WindowingData
import arx.engine.control.event.Event.KeyPressEvent
import arx.engine.control.event.Event.KeyReleaseEvent
import arx.engine.control.event.Event.MouseButton
import arx.engine.control.event.Event.MouseDragEvent
import arx.engine.control.event.Event.MouseMoveEvent
import arx.engine.control.event.Event.MousePressEvent
import arx.engine.control.event.Event.MouseReleaseEvent
import arx.engine.control.event.Event.ScrollEvent
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.GL

class WindowingControlComponent(controlEngine : ControlEngine) extends ControlComponent(controlEngine) {
	val WD = control[WindowingData]
	import WD._
	val WGD = graphics[WindowingGraphicsData]
	WGD.desktop = WD.desktop


	controlEngine.eventBus.onEvent {
		case kpe : KeyPressEvent =>
			focusedWidget.exists(w => w.handleEvent(kpe.copy().withOrigin(w)))
		case kre : KeyReleaseEvent =>
			focusedWidget.exists(w => w.handleEvent(kre.copy().withOrigin(w)))
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


	override protected def initialize(): Unit = {
//		graphics[WindowingGraphicsData].desktop = control[WindowingData].desktop
	}


	override protected def updateSelf(dt: UnitOfTime): Unit = {
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
				WD.desktop.selfAndChildren.find(w => {
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
			focusedWidget.foreach(w => w.handleEvent(FocusLostEvent(w)))
			focusedWidget = Some(newFocus)
			newFocus.handleEvent(FocusGainedEvent(newFocus))
		}
	}
}
