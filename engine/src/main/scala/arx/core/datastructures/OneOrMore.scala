package arx.core.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/8/13
 * Time: 2:32 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._

import arx.application.Noto
import arx.core.traits.TArxTraversable

abstract class OneOrMore[T] extends TArxTraversable[T]{
	def without ( v : T ) : Traversable[T]
	def toShortString : String
}


class ManyOneOrMore[T] ( intern : Traversable[T] ) extends OneOrMore[T] {
	def foreach[U](f: (T) => U): Unit = intern.foreach(f)
	def foreachUnsafe[U](f: (T) => U): Unit = intern.foreach(f)

	def without ( v : T ) = new ManyOneOrMore( intern.filterNot( _ == v ) )
	override def toList = intern match {
		case l : List[T] => l
		case _ => super.toList
	}


	override def toString(): String = toShortString

	override def toShortString: String = if (intern.size == 1) {
		intern.head.toString
	} else if (intern.size < 3) {
		"[" + intern.map(_.toString).reduceLeft(_ + ", " + _) + "]"
	} else {
		"[" + intern.take(3).map(_.toString).reduceLeft(_ + ", " + _) + ", ...]"
	}
}

class SingleOneOrMore[T] ( single : T ) extends OneOrMore[T] {
	def foreach[U](f: (T) => U): Unit = f(single)
	def foreachUnsafe[U](f: (T) => U): Unit = f(single)

	def without ( v : T ) = if ( v == single ) {
		Nil
	} else {
		this
	}
	override def toList = List(single)

	override def toString() = single.toString
	override def toShortString: String = toString()
}

object OneOrMore {
	implicit def fromTraversable[T] ( t : Traversable[T] ) = {
		if ( t.isEmpty ) { Noto.warn("OneOrMore is empty, that is very bad") }
		new ManyOneOrMore(t)
	}
	implicit def fromSingle[T] ( t : T ) = new SingleOneOrMore(t)
}


final class AutoTraversable[T](val intern : Traversable[T]) extends Traversable[T] {
	override def foreach[U](f: T => U): Unit = intern.foreach(f)
}
object AutoTraversable {
//	implicit def toTraversable[T] (autoT : AutoTraversable[T]) : Traversable[T] = autoT.intern
	implicit def fromTraversable[T] (t : Traversable[T]) : AutoTraversable[T] = new AutoTraversable[T](t)
	implicit def fromSingle[T] (t : T) : AutoTraversable[T] = new AutoTraversable[T](List(t))
}
