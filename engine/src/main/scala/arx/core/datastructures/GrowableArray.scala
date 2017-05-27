package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/21/12
 * Time: 1:41 PM
 * Created by nonvirtualthunk
 */

import java.util.Comparator

import arx.Prelude._
import arx.application.Noto
import java.util
import arx.core.THasSortKey
import scala.collection.generic.CanBuildFrom

class GrowableArray[T <: AnyRef](implicit man : Manifest[T]) extends Traversable[T] {
	var _size = 0
	var intern = manifest[T].newArray(16)

	override def size = _size

	def ensureSize ( n : Int ) {
		if ( intern.length < n ) {
			val newArray = manifest[T].newArray(math.max(intern.length*2,n))
			System.arraycopy(intern,0,newArray,0,intern.length)
			intern = newArray
		}
	}

	def append ( t : T ) {
		ensureSize(size+1)
		intern(size) = t
		_size += 1
	}

	@inline
	def apply ( i : Int ) = intern(i)
	@inline
	def update ( i : Int , t : T ) {
		intern(i) = t
		if (i >= _size) {
			_size = i+1
		}
	}

	def sortBy ( sortKeyFunc : (T) => Int ){
		val comparator = new FunctionComparator(sortKeyFunc)
		util.Arrays.sort(intern,0,size,comparator)
	}

	def foreach[U](f: (T) => U) {
		var i = 0; while ( i < size ) {
			f(intern(i))
		i += 1}
	}

	override def find(f : (T) => Boolean) : Option[T] = {
		var i = 0; while ( i < size ) {
			if ( f(intern(i)) ) {
				return Some(intern(i))
			}
		i += 1}
		None
	}

	def removeBySwapAndPop(v : T ) {
		var i = 0; while ( i < size ) {
			if ( intern(i) == v ) {
				swapAndPop(i)
				i = size
			}
		i += 1}
	}
	def swapAndPop(i : Int ) {
		intern(i) = intern(size-1) //pop and swap
		_size -= 1
	}
	def dropLast () { _size -= 1 }
	def indexWhere ( f : (T) => Boolean ) : Int = {
		var i = 0; while ( i < size ) {
			if ( f(intern(i)) ) {
				return i
			}
		i += 1}
		-1
	}
	def indexOf(t : T) : Int = {
		var i = 0; while ( i < size ) {
			if (intern(i) == t) {
				return i
			}
			i += 1}
		-1
	}

	def toArray : Array[T] = {
		val ret = Array.ofDim[T](size)
		Array.copy(intern,0,ret,0,size)
		ret
	}
}

class GrowableSortableArray[T <: THasSortKey : Manifest] extends GrowableArray[T] {
	def sort () {
		val comparator = new HasSortKeyComparator[T]
		util.Arrays.sort(intern,0,size,comparator)
	}

//	override def remove ( i : Int ) {
//		//We don't do a pop-and-swap here as we may have done in the normal growable array, since that would
//		//defeat any ordering that had been done
//
//		if ( i != _size - 1 ) { //If the one deleted is the last one, we don't care, just chop it off
//			System.arraycopy(intern,i+1,intern,i,size - i - 1)
//		}
//		_size -= 1
//	}
}

class GrowableFloatArray extends Traversable[Float] {
	private[this] final var _size = 0
	var intern = Array.ofDim[Float](16)

	override def size = _size

	def ensureSize ( n : Int ) {
		if ( intern.length < n ) {
			val newArray = Array.ofDim[Float](math.max(intern.length*2,n))
			System.arraycopy(intern,0,newArray,0,intern.length)
			intern = newArray
		}
	}

	def append ( t : Float ) {
		val s = _size
		ensureSize(s+1)
		intern(s) = t
		_size += 1
	}

	@inline
	def apply ( i : Int ) = intern(i)
	@inline
	def update ( i : Int , t : Float ) {
		intern(i) = t
		if (i <= _size) {
			_size = i+1
		}
	}

	def foreach[U](f: (Float) => U) {
		var i = 0; while ( i < size ) {
			f(intern(i))
			i += 1}
	}

	def remove ( v : Float ) {
		var i = 0; while ( i < size ) {
			if ( intern(i) == v ) {
				remove(i)
				i = size
			}
			i += 1}
	}
	def remove ( i : Int ) {
		intern(i) = intern(size-1) //pop and swap
		_size -= 1
	}
	def dropLast () { _size -= 1 }
	def indexWhere ( f : (Float) => Boolean ) : Int = {
		var i = 0; while ( i < size ) {
			if ( f(intern(i)) ) {
				return i
			}
			i += 1}
		-1
	}

	def clear(): Unit = {
		_size = 0
	}

	def toArray : Array[Float] = {
		val ret = Array.ofDim[Float](size)
		Array.copy(intern,0,ret,0,size)
		ret
	}
}

class FunctionComparator[T](func : (T) => Int) extends util.Comparator[T] {
	def compare(o1: T, o2: T) = {
		func(o1) - func(o2)
	}
}

class HasSortKeyComparator[T <: THasSortKey] extends util.Comparator[T]{
	def compare(o1: T, o2: T) = {
		val sortKey1 = o1.sortKey
		val sortKey2 = o2.sortKey
		if ( sortKey1 < sortKey2 ) { -1 }
		else if ( sortKey1 == sortKey2 ) { 0 }
		else { 1 }

	}
}