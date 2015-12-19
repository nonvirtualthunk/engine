package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/29/14
 * Time: 9:09 AM
 */

import arx.Prelude._
import scalaxy.loops._

class ArxVector[+T](val intern : Vector[T]) extends AnyVal {
	def insert[U >: T] (x : U, at : Int) : Vector[U] = {
		(intern.take(at) :+ x) ++ intern.takeRight(intern.size - at)
	}

	def ofType [E <: AnyRef : Manifest] : Vector[E] = {
		val erasure = manifest[E].erasure

		var ret : Vector[E] = Vector()
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			if ( erasure.isAssignableFrom(e.getClass) ) { ret :+= e.asInstanceOf[E] }
		}
		ret
	}
	def notOfType [E <: AnyRef : Manifest] : Vector[T] = {
		val erasure = manifest[E].erasure

		var ret : Vector[T] = Vector()
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			if ( ! erasure.isAssignableFrom(e.getClass) ) { ret :+= e }
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

	def without[U >: T] ( t : U ) : Vector[T] = intern.filterNot { e : T => e == t }

	def findOrElse[U >: T] ( predicate : (T) => Boolean , orElse : U ) = {
		intern.find(predicate).getOrElse(orElse)
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
}
