package arx.engine.control.components.windowing.widgets.data

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.core.vec.Vec3f
import arx.core.vec.Vec3i
import arx.core.vec.Vec4f
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.components.windowing.widgets.PositionExpression
import arx.engine.data.TAuxData
import arx.engine.data.THasAuxData
import arx.graphics.TToImage

import scalaxy.loops._

trait TWidgetAuxData extends TAuxData {

	def onAssignedToWidget(widget : Widget): Unit = {

	}

	override def onAssignedToObject(entity: THasAuxData[_]): Unit = {
		entity match {
			case w : Widget => onAssignedToWidget(w)
			case _ => Noto.error(s"Widget aux data used for non-widget entity: $entity")
		}
	}
}

class DragAndDropData extends TWidgetAuxData {
	var draggable = false
	var droppable = false
	var payload : Option[AnyRef] = None
}

object DragAndDropData {
	val Default = new DragAndDropData
}

// +====================+

class EventHandlingData extends TWidgetAuxData {
	var acceptsFocus = false
}

object EventHandlingData extends TWidgetAuxData {
	val Default = new EventHandlingData
}

// +====================+

class DrawingData extends TWidgetAuxData {
	var drawBackground = true
	var drawAsForegroundBorder = false
	var backgroundImage : Option[TToImage] = None
	var backgroundPixelScale = 1
	var backgroundColor = Vec4f.One
	var edgeColor = Vec4f.One
	var drawCenterBackground = true
	// note, these won't necessarily take effect if changed
	var interiorPadding : ReadVec2i = Vec2i.Zero
	var decorationBorderSize : ReadVec2i = Vec2i.Zero

	var shouldRedraw = false

	var relativePosition : ReadVec3i = Vec3i.Zero
	var absolutePosition : ReadVec3i = Vec3i.Zero
	var effectiveDimensions : ReadVec2i = Vec2i.One

//	var xWatcher = new Watcher[PositionExpression](PositionExpression.Flow)
//	var yWatcher = new Watcher[PositionExpression](PositionExpression.Flow)
//	var zWatcher = new Watcher[PositionExpression](PositionExpression.Flow)
//
//	var widthWatcher = new Watcher[DimensionExpression](DimensionExpression.Intrinsic)
//	var heightWatcher = new Watcher[DimensionExpression](DimensionExpression.Intrinsic)
//
//	override def onAssignedToWidget(widget: Widget): Unit = {
//		xWatcher = new Watcher(widget.x)
//		yWatcher = new Watcher(widget.y)
//		zWatcher = new Watcher(widget.z)
//
//		widthWatcher = new Watcher(widget.width)
//		heightWatcher = new Watcher(widget.height)
//	}
}

object DrawingData {
	val Default = new DrawingData
}