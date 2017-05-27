package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/27/12
 * Time: 10:51 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._

class RingBuffer[@specialized(Int,Short,Byte) T : Manifest](sizeHintPo2 : Int = 10) extends Traversable[T] {
	var sizePo2 = sizeHintPo2.max(1)
	var capAND = (1 << sizePo2) - 1
	var backingArray : Array[T] = manifest[T].newArray(1 << sizeHintPo2)
	var startIndex = 0
	var endIndex = 0

	def expand () {
		sizePo2 += 1
		capAND = (1 << sizePo2) - 1
		val newBackingArray = manifest[T].newArray(1 << sizePo2)
		if ( startIndex < endIndex ) {
			Array.copy(backingArray,startIndex,newBackingArray,0,endIndex - startIndex)
			startIndex = 0
			endIndex = endIndex - startIndex
		} else if ( endIndex < startIndex ) {
			val firstSectionLength = backingArray.length - startIndex
			val secondSectionLength = endIndex
			Array.copy(backingArray,startIndex,newBackingArray,0,firstSectionLength)
			Array.copy(backingArray,0,newBackingArray,firstSectionLength,secondSectionLength)
			startIndex = 0
			endIndex = firstSectionLength + secondSectionLength
		}
		backingArray = newBackingArray
	}

	def clear () { startIndex = 0; endIndex = 0 }

	override def isEmpty = startIndex == endIndex
	override def nonEmpty = startIndex != endIndex
	override def size = endIndex - startIndex
	def maxSize = 1 << sizePo2
	override def head = backingArray(startIndex)

	def enqueue( x : T, y : T ) { enqueue(x);enqueue(y); }
	def enqueue( x : T, y : T, z : T ) { enqueue(x);enqueue(y);enqueue(z) }
	def enqueue( x : T, y : T, z : T , a : T ) { enqueue(x);enqueue(y);enqueue(z);enqueue(a) }
	def enqueue( x : T, y : T, z : T , a : T , b : T ) { enqueue(x);enqueue(y);enqueue(z);enqueue(a);enqueue(b) }

	def enqueue( x : T ) {
		if ( endIndex < startIndex ) {
			if (((endIndex + 1) & capAND) >= startIndex ) { expand() }
		} else if ( endIndex > startIndex ) {
			if (((endIndex + 1) & capAND) == startIndex) { expand() }
		}


		backingArray(endIndex) = x
		endIndex = (endIndex + 1) & capAND
	}
	def dequeue() : T = {
		if ( startIndex == endIndex ) { throw new IllegalStateException("Ring buffer start passed end") }

		val ret = backingArray(startIndex)
		startIndex = (startIndex + 1) & capAND
		ret
	}

	/** Relative access, fetches from backing array relative to start Index */
	def get ( i : Int ) = backingArray((startIndex+i)&capAND)
	/** Relative access, stores to backing array relative to start Index */
	def set ( i : Int , x : T ) { backingArray((startIndex+i)&capAND) = x }

	/** Raw Access, fetches directly from the backing array */
	def apply ( i : Int ) = backingArray(i)
	/** Raw Mutation, sets directly to the backing array */
	def update ( i : Int , x : T ) { backingArray(i) = x }


	def foreach[U](f: (T) => U) {
		if ( startIndex < endIndex ) {
			var i = startIndex; while ( i < endIndex ) {
				f( backingArray(i) )
			i += 1}
		} else if ( endIndex < startIndex ) {
			var i = startIndex; while ( i < backingArray.length ) {
				f(backingArray(i))
			i += 1}
			i = 0; while ( i < endIndex ) {
				f(backingArray(i))
			i += 1}
		}
	}
}