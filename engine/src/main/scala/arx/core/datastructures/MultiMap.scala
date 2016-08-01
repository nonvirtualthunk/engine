package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/30/12
 * Time: 12:14 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.mutable
import collection.mutable.ListBuffer

class MultiMap[K,V] {
	val intern = new mutable.HashMap[K,ListBuffer[V]]
	def add ( k : K , v : V ) {
		intern.getOrElseUpdate(k,new ListBuffer[V]).append(v)
	}
	def addAll ( k : K , v : Traversable[V] ) { for ( subV <- v ) { add(k,subV) } }
	def addAll[U <: K] (m : MultiMap[U,V]): Unit =  {
		for ((k,vlist) <- m.intern) {
			this.addAll(k, vlist)
		}
	}
	def remove ( k : K , v : V ) : Boolean = {
		val buf = intern.getOrElse(k,new ListBuffer[V])
		buf.indexOf(v) match {
			case -1 => false
			case i => buf.remove(i); true
		}
	}
	def fetchAndRemoveHead ( k : K ) : Option[V] = {
		val buf = intern.getOrElse(k,new ListBuffer[V])
		if ( buf.isEmpty ) { None }
		else {
			val ret = buf.head
			buf.remove(0)
			Some(ret)
		}
	}
	def removeAll ( k : K ) { intern.remove(k) }
	def clear () { intern.clear() }
	def contains ( k : K ) = intern.contains(k)

	/**
	 * Returns all values that match the given key, if any, or Nil otherwise
	 * @param k
	 * @return
	 */
	def get ( k : K ) = intern.getOrElse(k,Nil)

	def toList : List[(K,ListBuffer[V])] = intern.toList
	def toCountList : List[(K,Int)] = intern.toList.map( tup => tup._1 -> tup._2.size )

	def values = intern.values
	def flattenedValues : Traversable[V] = {
		new Traversable[V] {
			def foreach[U](f: (V) => U): Unit = {
				val iter = intern.values.iterator
				while ( iter.hasNext ) {
					val lb = iter.next()
					val subIter = lb.iterator
					while ( subIter.hasNext ) {
						val v = subIter.next()
						f(v)
					}
				}
			}

			override def size: Int = {
				var sum = 0
				val iter = intern.values.iterator
				while ( iter.hasNext ) {
					sum += iter.next().size
				}
				sum
			}
		}
	}
	def flattenedValuesIterator : Iterator[V] = {
		new Iterator[V] {
			protected val iter = intern.values.iterator
			protected var subIter : Iterator[V] = null

			protected def advance () {
				while ( (subIter == null || ! subIter.hasNext) && iter.hasNext ) {
					subIter = iter.next().iterator
				}
			}
			advance()

			def hasNext: Boolean = subIter != null && subIter.hasNext

			def next(): V = {
				val v = subIter.next()
				advance()
				v
			}
		}
	}
}
object MultiMap {
	def empty[K,V] = new MultiMap[K,V]()

	def from[K,V](m : Map[K,V]) = {
		val ret = new MultiMap[K,V]
		for ((k,v) <- m) {
			ret.add(k,v)
		}
		ret
	}

	def apply[K,V](tups : (K,V)*) = {
		val ret = new MultiMap[K,V]
		for ((k,v) <- tups) {
			ret.add(k,v)
		}
		ret
	}
}