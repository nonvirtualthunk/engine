package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:32 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.Prelude
import arx.core.units.UnitOfMeasure
import scala.collection.parallel.{ParIterableLike, TaskSupport}

class ArxList[+T](val intern : List[T]) extends AnyVal{
	def ofType [E <: AnyRef : Manifest] : List[E] = {
		val erasure = manifest[E].erasure

		var ret : List[E] = Nil
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			if ( erasure.isAssignableFrom(e.getClass) ) { ret ::= e.asInstanceOf[E] }
		}
		ret
	}
	def notOfType [E <: AnyRef : Manifest] : List[T] = {
		val erasure = manifest[E].erasure

		var ret : List[T] = Nil
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			if ( ! erasure.isAssignableFrom(e.getClass) ) { ret ::= e }
		}
		ret
	}
	def findFirstWith[U] ( f : (T) => Option[U] ) : Option[(T,U)] = {
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			f(e) match {
				case Some(v) => return Some((e,v))
				case _ =>
			}
		}
		None
	}
	def without[U >: T] ( t : U ) : List[T] = intern.filterNot { e : T => e == t }

	def sliding2: Iterator[(T,T)] = {
		if (intern.length > 1) {
			intern.sliding(2).map(i => (i(0), i(1)))
		} else {
			Iterator.empty
		}
	}

	def findOrElse[U >: T] ( predicate : (T) => Boolean , orElse : U ) = {
		intern.find(predicate).getOrElse(orElse)
	}

	def parTS ( ts : TaskSupport ) = {
		val base = intern.par
		base match {
			case pil : ParIterableLike[_,_,_] => pil.tasksupport = ts
			case _ =>
		}
		base
	}

	def insert[U >: T] (x : U, at : Int) = {
		intern.take(at) ::: x :: intern.takeRight(intern.size - at)
	}

	def maxByWithValue[U >: T] (f : (U => Float)) = {
		if (intern.isEmpty) {throw new UnsupportedOperationException("empty.maxBy")}
		var maxElem : T = intern.head
		var maxWeight : Float = f(maxElem)
		for (v <- intern.tail) {
			val weight = f(v)
			if (weight > maxWeight) {
				maxWeight = weight
				maxElem = v
			}
		}
		maxElem -> maxWeight
	}

	def cross[U >: T] (other : Traversable[U], join : (U,U) => U) = {
		for (x <- intern; y <- other) yield {
			join(x,y)
		}
	}
}