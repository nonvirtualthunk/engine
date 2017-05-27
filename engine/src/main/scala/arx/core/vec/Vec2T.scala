package arx.core.vec

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import scalaxy.loops._

case class Vec2T[T](var x : T, var y : T) {
	def apply(i : Int) = i match {
		case 0 => x
		case 1 => y
		case _ => throw new IndexOutOfBoundsException
	}

	def update(i: Int, t : T) : Unit = i match {
		case 0 => x = t
		case 1 => y = t
		case _ => throw new IndexOutOfBoundsException
	}
}

case class Vec3T[T](var x : T, var y : T, var z : T) {
	def apply(i : Int) = i match {
		case 0 => x
		case 1 => y
		case 2 => z
		case _ => throw new IndexOutOfBoundsException
	}

	def update(i: Int, t : T) : Unit = i match {
		case 0 => x = t
		case 1 => y = t
		case 2 => z = t
		case _ => throw new IndexOutOfBoundsException
	}
}
