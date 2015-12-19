package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/20/13
 * Time: 11:38 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfMeasure

class ArxIterable[+T](intern : Iterable[T]) {
	def firstOfType [E <: AnyRef : Manifest] : Option[E] = {
		val iter = intern.iterator
		while ( iter.hasNext ) {
			val n = iter.next()
			n match {
				case e if ( manifest[E].erasure.isAssignableFrom(e.getClass) ) => { return Some(e.asInstanceOf[E]) }
				case _ =>
			}
		}
		None
//			this.ofType[E].headOption
	}

	def usum[U <: UnitOfMeasure[U]]( f : (T) => U )(implicit start : U) : U = { intern.map( v => f(v) ).foldLeft(start)( (a,b) => a + b ) }
	def fsum ( f : (T) => Float ) : Float = { intern.foldLeft(0.0f) { (a,v) => a + f(v) } }
	def isum ( f : (T) => Int ) : Int = { intern.foldLeft(0) { (a,v) => a + f(v) } }
	def fmin (f : (T) => Float ) : Float = intern.tail.foldLeft(f(intern.head)) { (a,v) => math.min(a,f(v)) }
	def fmax (f : (T) => Float ) : Float = intern.tail.foldLeft(f(intern.head)) { (a,v) => math.max(a,f(v)) }
	def imin (i : (T) => Int) : Int = intern.tail.foldLeft(i(intern.head)) { (a,v) => math.min(a,i(v)) }
	def imax (i : (T) => Int) : Int = intern.tail.foldLeft(i(intern.head)) { (a,v) => math.max(a,i(v)) }
	def maxOrElse[U >: T] (f : U)(implicit ord : Ordering[U]) : U = if (intern.isEmpty) { f } else { intern.max(ord) }
}