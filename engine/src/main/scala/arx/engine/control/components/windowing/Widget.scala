package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.datastructures.Watcher
import arx.core.vec.{Vec2T, Vec3T}
import arx.engine.control.components.windowing.widgets.DimensionExpression.Intrinsic
import arx.engine.control.components.windowing.widgets.{DimensionExpression, PositionExpression}
import arx.engine.control.components.windowing.widgets.PositionExpression.Flow
import arx.engine.control.components.windowing.widgets.data.{DragAndDropData, DrawingData, EventHandlingData, TWidgetAuxData}
import arx.engine.control.event.Event.{Event, TEventUser}
import arx.engine.data.THasInternalAuxData

class Widget(val _parent : Widget) extends TEventUser with THasInternalAuxData[TWidgetAuxData] {
	def parent = _parent
	var identifier : String = {
		val ret = "widget" + Widget.counter
		Widget.counter += 1
		ret
	}
	var children : List[Widget] = List()

	val position = Vec3T[PositionExpression](Flow,Flow,Flow)
	var dimensions = Vec2T[DimensionExpression](Intrinsic,Intrinsic)
	var showing = Moddable(true)
	protected var _isModified = false

	protected val stateWatcher = new Watcher(showing.resolve())

	// initialization =====================================
	if (parent != null) {
		var topmostParent = parent
		while (topmostParent.parent != null) {
			topmostParent = topmostParent.parent
		}
		topmostParent.synchronized {
			parent.children ::= this
		}
	}

	// ================================= end initialization

	// information related to dragging and dropping
	def dragAndDropRO = this.auxDataOrElse[DragAndDropData](DragAndDropData.Default)
	def eventHandlingRO = this.auxDataOrElse[EventHandlingData](EventHandlingData.Default)
	def drawing = this.auxData[DrawingData]
	def isModified = _isModified || isSelfModified || stateWatcher.hasChanged
	protected[windowing] def isSelfModified = false
	def resetModified() = _isModified = false

	def updateSelf(){}


	def x = position.x
	def x_= (t : PositionExpression) = position.x = t
	def y = position.y
	def y_= (t : PositionExpression) = position.y = t
	def z = position.z
	def z_= (t : PositionExpression) = position.z = t

	def width = dimensions.x
	def width_= (t : DimensionExpression) = dimensions.x = t
	def height = dimensions.y
	def height_= (t : DimensionExpression) = dimensions.y = t

	eventFallback {
		case e : Event => parent.handleEvent(e)
	}

	def selfAndAncestors : List[Widget] = this :: parent.selfAndAncestors
	def selfAndChildren : Iterable[Widget] = Iterable(this) ++ children.flatMap(_.selfAndChildren)

	def close (): Unit = {
		parent.children = parent.children.without(this)
		parent._isModified = true
	}
}

object Widget {
	var counter = 0
}