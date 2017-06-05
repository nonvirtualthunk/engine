package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.Cardinals
import arx.engine.control.components.windowing.Widget

import scalaxy.loops._

trait PositionExpression {
	def dependsOn : List[Widget] = Nil
}

trait DimensionExpression {

}

sealed class WindowingOrientation {}
case object TopLeft extends WindowingOrientation
case object BottomRight extends WindowingOrientation
case object TopRight extends WindowingOrientation
case object BottomLeft extends WindowingOrientation

object PositionExpression {
	case class Constant(value : Int, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression
	case class Proportional(proportion : Float, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression
	case object Centered extends PositionExpression
	case class Relative(relativeTo: Widget, offset : Int, direction : Int = Cardinals.Right) extends PositionExpression {
		override def dependsOn = List(relativeTo)
	}
	case object Flow extends PositionExpression
}

object DimensionExpression {
	case class Constant(value : Int) extends DimensionExpression
	case class Proportional(proportion : Float) extends DimensionExpression
	case class Relative(delta : Int) extends DimensionExpression
	case object Intrinsic extends DimensionExpression
}
