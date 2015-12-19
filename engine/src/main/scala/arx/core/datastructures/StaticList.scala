package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/8/13
 * Time: 12:49 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.immutable.{VectorPointer, Vector, IndexedSeq}
import collection.generic.{SeqFactory, GenericCompanion, GenericTraversableTemplate}
import scala.Vector
import collection.{Iterator, mutable, IndexedSeqLike}
import annotation.unchecked.uncheckedVariance
import collection.mutable.ArrayBuffer

//@SerialVersionUID(1L)
//class StaticList[A] ( backingArray : Array[A] ) extends IndexedSeq[A]
//   with GenericTraversableTemplate[A, StaticList]
//   with IndexedSeqLike[A, StaticList[A]]
//   with Serializable
//{
//	def length: Int = size
//	def apply(idx: Int): A = {
//		backingArray(idx)
//	}
//
//
//	override def lengthCompare(len: Int): Int = length - len
//	@inline override def iterator: Iterator[A] = new StaticListIterator(0,this)
//
//	override def foreach[U](f: (A) => U) {
//		var i = 0; while ( i < backingArray.length ) {
//			f( backingArray(i) )
//			i += 1
//		}
//	}
//}

//object StaticList extends SeqFactory[StaticList] {
//	def newBuilder[A]: mutable.Builder[A, StaticList[A]] = new ImmutableArrayBuilder
//
//	val NIL = new StaticList(null)
//	override def empty[A]: StaticList[A] = NIL.asInstanceOf[StaticList[A]]
//}
//
//class ImmutableArrayBuilder[A] extends mutable.Builder[A, StaticList[A]] {
//	var runningBuffer = new ArrayBuffer[A]
//
//	def +=(elem: A): ImmutableArrayBuilder[A] = {
//		runningBuffer += elem
//		this
//	}
//
//	def clear() {
//		runningBuffer.clear()
//	}
//
//	def result(): StaticList[A] = {
//		new StaticList( runningBuffer.toArray )
//	}
//}

//class StaticListIterator[+A](var index : Int,immutableArray : StaticList[A]) extends Iterator[A] {
//	def hasNext: Boolean = index < immutableArray.length - 1
//	def next(): A = {
//		if ( ! hasNext ) { throw new NoSuchElementException("Iterator does not have next element") }
//		val ret = immutableArray(index)
//		index += 1
//		ret
//	}
//}