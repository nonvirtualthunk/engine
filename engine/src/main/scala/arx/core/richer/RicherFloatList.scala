package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:30 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

class RicherFloatList[T] ( val l : List[T] ) {
	def avg(implicit evidence : T => Float) : Float = { l match {
		case Nil => 0.0f
		case _ => l.map{ a => a : Float }.sum / l.size.toFloat
	}}
	def foldMult(base:Float)(implicit evidence : T => Float) : Float = { l.foldLeft(base)(_ * _) }
	def minOr ( f : T )(implicit cmp : scala.Ordering[T]) = l match {
		case Nil => f
		case other => other.min
	}
	def maxOr ( f : T )(implicit cmp : scala.Ordering[T]) = l match {
		case Nil => f
		case other => other.max
	}
}