package arx.core.datastructures.primitive

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/10/13
 * Time: 2:00 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import java.util

/**
 * NOTE : As of now, this class is not as fast as the original, parameterized fibonacci heap...my guess is
 * that as the number of nodes gets larger, the cache misses from the array data being fragmented overcomes
 * any potential gains. Leaving this here out of interest, and/or possible future improvements.
 */
class FibonacciHeap {
	private[this] var min : Int = -1

	//+====================+ Node Data +====================+


	//+====================+ </ Node Data /> +====================+



	def clear () {
		min = -1
		size = 0
		numFreeIndices = 0
	}

	def decreaseKey ( x : Int , k : Float ) {
		//Delete is only used as a utility by other sections of internal code, outside can't trigger it
		decreaseKey(x,k,delete = false)
	}

	protected def decreaseKey ( x : Int , k : Float, delete : Boolean ) {
		if ( ! delete && k > key(x) ) {
			throw new IllegalArgumentException("cannot increase key value");
		}
		key(x) = k
		val y = parent(x)
		if ( y != -1 && (delete || k < key(y)) ) {
			cut(y,x,min)
			cascadingCut(y,min)
		}

		if ( delete || k < key(min) ) {
			min = x
		}
	}

	def delete ( x : Int ) {
		decreaseKey(x,Float.MinValue,delete = true)
		removeMin()
	}

	def isEmpty = min == -1

	def insert ( datum : Int , newKey : Float ) = {
		val node = createNode(datum,newKey)

		if ( min != -1 ) {
			right(node) = min
			left(node) = left(min)
			left(min) = node
			right(left(node)) = node
			if ( newKey < key(min) ) {
				min = node
			}
		} else {
			min = node
		}
		node
	}

	def minDatum = data(min)
	def removeMin () : Int = {
		val z = min
		if ( z == -1 ) {
			return -1
		}
		if ( child(z) != -1 ) {
			parent(child(z)) = -1
			var x = right(child(z))
			while ( x != child(z) ) {
				parent(x) = -1

				x = right(x)
			}

			val minLeft = left(min)
			val zChildLeft = left(child(z))
			left(min) = zChildLeft
			right(zChildLeft) = min
			left(child(z)) = minLeft
			right(minLeft) = child(z)
		}
		right(left(z)) = right(z)
		left(right(z)) = left(z)
		if ( z == right(z) ) {
			min = -1
		} else {
			min = right(z)
			consolidate()
		}

		deleteNode(z)

		data(z)
	}


	// The magic 45 comes from log base phi of Integer.MAX_VALUE,
	// which is the most elements we will ever hold, and log base
	// phi represents the largest degree of any root list node.
	private[this] val consolidationArray = Array.ofDim[Int](45)
	protected def consolidate () {
		util.Arrays.fill(consolidationArray,-1)

		var start = min
		var w = min
		do {
			var x = w
			// Because x might be moved, save its sibling now.
			var nextW = right(w)
			var d = degree(x)
			while ( consolidationArray(d) != -1 ) {
				// Make one of the nodes a child of the other.
				var y = consolidationArray(d)
				if ( key(x) > key(y) ) {
					val temp = y
					y = x
					x = temp
				}
				if ( y == start ) {
					// Because removeMin() arbitrarily assigned the min
					// reference, we have to ensure we do not miss the
					// end of the root node list.
					start = right(start)
				}
				if ( y == nextW ) {
					// If we wrapped around we need to check for this case.
					nextW = right(nextW)
				}
				// Node y disappears from root list.
				link(y,x)
				// We've handled this degree, go to next one.
				consolidationArray(d) = -1
				d += 1
			}
			// Save this node for later when we might encounter another
			// of the same degree.
			consolidationArray(d) = x
			// Move forward through list.
			w = nextW
		} while ( w != start )

		// The node considered to be min may have been changed above.
		min = start
		var q = 0; while ( q < consolidationArray.length ) {
			val a = consolidationArray(q)
			if ( a != -1 && key(a) < key(min) ) {
				min = a
			}
		q += 1}
	}


	//+====================+ Raw Data Stuff +====================+
	private[this] var size = 0
	private[this] var space : Int = 128

	private[this] var data = Array.ofDim[Int](space)
	private[this] var key = Array.ofDim[Float](space)
	private[this] var parent = Array.ofDim[Int](space)
	util.Arrays.fill(parent,size,parent.length,-1)
	private[this] var child = Array.ofDim[Int](space)
	util.Arrays.fill(child,size,child.length,-1)
	private[this] var right = Array.ofDim[Int](space)
	private[this] var left = Array.ofDim[Int](space)
	private[this] var degree = Array.ofDim[Int](space)
	private[this] var mark = Array.ofDim[Boolean](space)
	private[this] var freeIndices = Array.ofDim[Int](space)
	private[this] var numFreeIndices = 0

	protected def createNode ( newDatum : Int , newKey : Float ) = {
		var newIndex = -1

		if ( numFreeIndices > 0 ) {
			newIndex = freeIndices(numFreeIndices-1)
			numFreeIndices -= 1
		} else {
			if ( space <= size ) {
				space *= 2
				data = copyArray(data, Array.ofDim(space))
				key = copyArray(key, Array.ofDim(space))
				parent = copyArray(parent, Array.ofDim(space))
//				util.Arrays.fill(parent,size,parent.length,-1) //Fill with our equivalent of null
				child = copyArray(child, Array.ofDim(space))
//				util.Arrays.fill(child,size,child.length,-1) //Fill with our equivalent of null
				left = copyArray(left, Array.ofDim(space))
				right = copyArray(right, Array.ofDim(space))
				degree = copyArray(degree, Array.ofDim(space))
				mark = copyArray(mark, Array.ofDim(space))
			}
			newIndex = size
		}

		parent(newIndex) = -1
		child(newIndex) = -1
		data(newIndex) = newDatum
		key(newIndex) = newKey
		right(newIndex) = newIndex
		left(newIndex) = newIndex
		degree(newIndex) = 0
		mark(newIndex) = false

		size += 1
		newIndex
	}

	protected def deleteNode ( index : Int ) {
		if ( freeIndices.length <= numFreeIndices ) {
			freeIndices = copyArray(freeIndices, Array.ofDim(freeIndices.length * 2))
		}
		freeIndices( numFreeIndices ) = index
		numFreeIndices += 1

		size -= 1
	}

	protected def cascadingCut ( cutNode : Int , minNode : Int ) {
		val z = parent(cutNode)
		if ( z != -1 ) {
			if ( mark(cutNode) ) {
				// it's marked, cut it from parent
				cut(z,cutNode,minNode)
				// cut its parent as well
				cascadingCut(z,minNode)
			} else {
				mark(cutNode) = true
			}
		}
	}

	protected def cut ( thisNode : Int , x : Int , minNode : Int ) {
		// remove x from childlist and decrement degree
		right(left(x)) = right(x)
		left(right(x)) = left(x)
		degree(thisNode) -= 1

		// reset child if necessary
		if ( degree(thisNode) == 0 ) {
			child(thisNode) = -1
		} else if ( child(thisNode) == x ) {
			child(thisNode) = right(x)
		}
		// add x to root list of heap
		right(x) = minNode
		left(x) = left(minNode)
		left(minNode) = x
		right(left(x)) = x
		// set parent[x] to nil
		parent(x) = -1
		// set mark[x] to false
		mark(x) = false
	}

	protected def link ( thisNode : Int , parentNode : Int ) {
		right(left(thisNode)) = right(thisNode)
		left(right(thisNode)) = left(thisNode)
		//make this a child of x
		parent(thisNode) = parentNode
		if ( child(parentNode) == -1 ) {
			child(parentNode) = thisNode
			right(thisNode) = thisNode
			left(thisNode) = thisNode
		} else {
			left(thisNode) = child(parentNode)
			right(thisNode) = right(child(parentNode))
			right(child(parentNode)) = thisNode
			left(right(thisNode)) = thisNode
		}

		//increase degree[x]
		degree(parentNode) += 1
		// set mark false
		mark(thisNode) = false
	}

	protected def copyArray[T]( arr1 : Array[T], arr2 : Array[T] ) = {
		Array.copy(arr1,0,arr2,0,arr1.length)
		arr2
	}
}

object FibonacciHeap {
	private[this] class NodeContainer {

	}
}