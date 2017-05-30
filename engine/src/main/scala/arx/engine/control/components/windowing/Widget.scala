package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import java.util.UUID

import arx.Prelude._
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2T
import arx.core.vec.Vec2i
import arx.core.vec.Vec3T
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.components.windowing.widgets.DimensionExpression.Intrinsic
import arx.engine.control.components.windowing.widgets.PositionExpression
import arx.engine.control.components.windowing.widgets.PositionExpression.Flow
import arx.engine.control.components.windowing.widgets.data.DragAndDropData
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.control.components.windowing.widgets.data.EventHandlingData
import arx.engine.control.components.windowing.widgets.data.TWidgetAuxData
import arx.engine.control.event.Event.Event
import arx.engine.control.event.Event.TEventUser
import arx.engine.data.THasInternalAuxData
import org.pybee.cassowary.Constraint
import org.pybee.cassowary.Expression
import org.pybee.cassowary.StayConstraint
import org.pybee.cassowary.Strength
import org.pybee.cassowary.Variable

import scalaxy.loops._

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

	// initialization =====================================
	if (parent != null) {
		parent.children ::= this
	}
	// ================================= end initialization

	// information related to dragging and dropping
	def dragAndDropRO = this.auxDataOrElse[DragAndDropData](DragAndDropData.Default)
	def eventHandlingRO = this.auxDataOrElse[EventHandlingData](EventHandlingData.Default)
	def drawing = this.auxData[DrawingData]
	def isModified = false


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
	}
}

object Widget {
	var counter = 0
}