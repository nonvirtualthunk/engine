package arx.engine.lworld

import arx.Prelude.toRicherNumeric
import arx.core.math.Sext

sealed trait Impact

object Impact {

	case object Positive extends Impact

	case object Negative extends Impact

	case object Neutral extends Impact

	def apply[T: Numeric](n: T): Impact = implicitly[Numeric[T]].compare(n, implicitly[Numeric[T]].zero) match {
		case i if i > 0 => Impact.Positive
		case i if i < 0 => Impact.Negative
		case _ => Impact.Neutral
	}

	def fromBeforeAfterAny(oldV : Any, newV : Any) : Option[Impact] = {
		if (oldV.getClass == newV.getClass) {
			 val comparisonResult = oldV match {
				 case i : Int => Some(i.compareTo(newV.asInstanceOf[Int]))
				 case f : Float => Some(f.compareTo(newV.asInstanceOf[Float]))
				 case d : Double => Some(d.compareTo(newV.asInstanceOf[Double]))
				 case s : Short => Some(s.compareTo(newV.asInstanceOf[Short]))
				 case s : Sext => Some(s.compareTo(newV.asInstanceOf[Sext]))
				 case _ => None
			 }

			comparisonResult.map(r => if (r < 0) {
				Positive
			} else if (r > 0) {
				Negative
			} else {
				Neutral
			})
		} else {
			None
		}
	}
}

case class BreakdownElement[T](source: Option[String], effect: String, impact: Impact)

case class Breakdown[T](total : T, elements : Vector[BreakdownElement[T]]) {

	def bonus[U <: T : Numeric](delta : U, source: String, ignoreZero : Boolean = true) : Breakdown[T] = delta.compare0 match {
		case -1 => malus(delta.negate, source)
		case 0 if ignoreZero => this
		case _ => this.copy(implicitly[Numeric[U]].plus(total.asInstanceOf[U], delta), elements :+ BreakdownElement(Some(source), s"+ $delta", Impact.Positive))
	}

	def malus[U <: T : Numeric](delta : U, source: String, ignoreZero : Boolean = true) : Breakdown[T] = delta.compare0 match {
		case -1 => bonus(delta.negate, source)
		case 0 if ignoreZero => this
		case _ => this.copy(implicitly[Numeric[U]].plus(total.asInstanceOf[U], delta), elements :+ BreakdownElement(Some(source), s"- $delta", Impact.Positive))
	}

	def mergedWith(other : Breakdown[T], sourcePrefix : Option[String], mergeFunc : (T,T) => T) : Breakdown[T] = {
		val transformedOtherElements = sourcePrefix match {
			case Some(prefix) => other.elements.map(e => e.copy(source = e.source.map(src => s"$prefix $src")))
			case None => other.elements
		}
		Breakdown(mergeFunc(this.total, other.total), this.elements ++ transformedOtherElements.asInstanceOf[Vector[BreakdownElement[T]]])
	}
}
object Breakdown {
	def apply[T](value : T, source : String, effect : String, impact : Impact) : Breakdown[T] =
		Breakdown(value, Vector(BreakdownElement(Some(source), effect, impact)))

	def bonus[T : Numeric](value : T, source : String, ignoreZero : Boolean = true) : Breakdown[T] = value.compare0 match {
		case -1 => malus(value.negate, source, ignoreZero)
		case 0 if ignoreZero => Breakdown(value, Vector())
		case 1 => Breakdown(value, Vector(BreakdownElement(Some(source), s"+ $value", Impact.Positive)))
	}

	/**
	  * a positive value in this context means a malus of that amount. I.e. malus(3) indicates that the value should be reduced by 3
	  */
	def malus[T : Numeric](value : T, source : String, ignoreZero : Boolean = true) : Breakdown[T] = value.compare0 match {
		case -1 => bonus(value.negate, source, ignoreZero)
		case 0 if ignoreZero => Breakdown(value, Vector())
		case 1 => Breakdown(value, Vector(BreakdownElement(Some(source), s"- $value", Impact.Negative)))
	}

	def empty[T : Numeric]() : Breakdown[T] = {
		Breakdown(implicitly[Numeric[T]].zero, Vector())
	}
}