package arx.core.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/13
 * Time: 11:30 AM
 */

import arx.Prelude._
import scalaxy.loops._

/**
 * A trait for any object that denotes a set of things and can test for inclusion. All
 * Traversable's are capable of so doing (by means of <code>.contains(...)</code>), but
 * this also applies to a function that takes <code>T</code> and returns a boolean.
 */
trait TInclusionSet[T] {
	def contains ( t : T ) : Boolean
}
object TInclusionSet {
	implicit def fromSet[T] ( trav : Set[T] ) : TInclusionSet[T] = new TInclusionSet[T] {
		def contains(t: T): Boolean = trav.contains(t)
	}
	implicit def fromList[T] ( trav : List[T] ) : TInclusionSet[T] = new TInclusionSet[T] {
		def contains(t: T): Boolean = trav.contains(t)
	}
	implicit def fromTraversable[T] ( trav : Traversable[T] ) : TInclusionSet[T] = new TInclusionSet[T] {
		def contains(t: T): Boolean = trav.exists( e => e == t )
	}
	implicit def fromFunction [T] ( f : (T) => Boolean ) : TInclusionSet[T] = new TInclusionSet[T] {
		def contains(t: T): Boolean = f(t)
	}
	implicit def fromSingle [T] ( v : T ) = new TInclusionSet[T] {
		def contains(t: T): Boolean = t == v
	}
}
