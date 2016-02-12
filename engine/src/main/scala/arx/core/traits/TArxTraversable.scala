package arx.core.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/13
 * Time: 10:55 AM
 */

import arx.Prelude._
import scalaxy.loops._

trait TArxTraversable[T] extends Traversable[T] {
	/**
	 * As <code>foreach</code> but the values passed to the function are
	 * <strong>not</strong> guaranteed to remain valid any longer than the
	 * duration of a single execution of <code>f</code>. This allows a collection
	 * of <code>VoxelCoord</code>'s to avoid instantiating many short lived
	 * objects by simply passing a single <code>MutableVoxelCoord</code> on
	 * each iteration. In such a case it would be extremely inadvisable to
	 * store the passed value elsewhere for later usage.
	 */
	def foreachUnsafe[U] ( f : (T) => U )

	def existsUnsafe ( f : (T) => Boolean ) : Boolean = {
		import scala.util.control.Breaks._
		var result = false
		breakable {
			foreachUnsafe( value => {
				if ( f(value) ) {
					result = true
					break()
				}
			} )
		}
		result
	}

	def forallUnsafe ( f : (T) => Boolean ) : Boolean = {
		! existsUnsafe( v => ! f(v) )
	}


	def filtered ( filter : (T) => Boolean ) = new FilteredArxTraversable[T](this,filter)

	override def equals(p1: scala.Any): Boolean = p1 match {
		case t : Traversable[_] => t.toList == this.toList
		case _ => false
	}
	override def hashCode = {
		var summingHash = 0
		this.foreachUnsafe( v => summingHash = summingHash * 31 + v.hashCode )
		summingHash
	}
}

trait TSafetyPromotableArxTraversable[T] extends TArxTraversable[T] {
	/** As filter, but the user must ensure that the reference provided to the filter function never escapes
	  * since it can be mutable and may not retain its value */
	def filterUnsafe (f : (T) => Boolean) : TArxTraversable[T] = {
		var ret = Vector[T]()
		foreachUnsafe(value => if (f(value)) { ret :+= toImmutable(value) })
		ret
	}

	def toImmutable( t : T ) : T
}

class FilteredArxTraversable[T](intern : TArxTraversable[T], filter : (T) => Boolean) extends TArxTraversable[T] {
	override def size = intern.size

	def foreachUnsafe[U](f: (T) => U): Unit = {
		intern.foreachUnsafe( (t) => {
			if ( filter(t) ) {
				f(t)
			}
		})
	}

	def foreach[U](f: (T) => U): Unit = {
		intern.foreach( (t) => {
			if ( filter(t) ) {
				f(t)
			}
		})
	}
}

object TArxTraversable{
	implicit class Wrapper[T] ( val intern : Traversable[T] ) extends TArxTraversable[T] {
		/**
		 * As <code>foreach</code> but the values passed to the function are
		 * <strong>not</strong> guaranteed to remain valid any longer than the
		 * duration of a single execution of <code>f</code>. This allows a collection
		 * of <code>VoxelCoord</code>'s to avoid instantiating many short lived
		 * objects by simply passing a single <code>MutableVoxelCoord</code> on
		 * each iteration. In such a case it would be extremely inadvisable to
		 * store the passed value elsewhere for later usage.
		 */
		def foreachUnsafe[U](f: (T) => U): Unit = intern.foreach(f)

		def foreach[U](f: (T) => U): Unit = intern.foreach(f)
	}
}