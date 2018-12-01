package arx.core.algorithms

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import scalaxy.loops._

object Algorithms {

}



class A[T](val b : Int, val c : Int, q : T) {
	val d = List.fill(b * c)(q)
	def e (f : Int, g : Int) : T = {
		f match {
			case h if h >= 0 && h < b => g match {
				case i if i >= 0 && i < c => {
					d(h * c + i)
				}
				case _ => throw new IllegalStateException("bad")
			}
			case _ => throw new IllegalStateException("bad")
		}
	}
}