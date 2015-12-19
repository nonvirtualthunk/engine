package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/4/12
 * Time: 10:26 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.mutable

class SynchronizedQueue[T] extends mutable.Queue[T] {
	def dequeueOpt() = {
		synchronized {
			if ( ! super.isEmpty ) { Some(super.dequeue()) }
			else { None }
		}
	}


	override def contains[A1 >: T](elem: A1): Boolean = { synchronized {super.contains (elem)} }
	override def clear () { synchronized { super.clear() } }
	override def dequeue() = synchronized { super.dequeue() }
	override def nonEmpty = synchronized { super.nonEmpty }
	override def isEmpty = synchronized { super.isEmpty }
	override def enqueue ( elems : T*) { synchronized[this.type] { super.++=(elems) } }
	override def += ( elem : T ) = { synchronized[this.type] { super.+=(elem) } }
	override def ++= ( elems : TraversableOnce[T]) = { synchronized[this.type] { super.++=(elems) } }
}