package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import scalaxy.loops._

trait PositionExpression {

}

trait DimensionExpression {

}

sealed trait WindowingOrientation {}
case object TopLeft extends WindowingOrientation
case object BottomRight extends WindowingOrientation

object PositionExpression {
	case class Constant(value : Int, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression
	case class Proportional(proportion : Float, relativeTo : WindowingOrientation = TopLeft) extends PositionExpression
	case object Flow extends PositionExpression
}

object DimensionExpression {
	case class Constant(value : Int) extends DimensionExpression
	case class Proportional(proportion : Float) extends DimensionExpression
	case class Relative(delta : Int) extends DimensionExpression
	case object Intrinsic extends DimensionExpression
}
