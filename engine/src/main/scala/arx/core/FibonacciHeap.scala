package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/12/12
 * Time: 10:34 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

trait THasSortKey {
	def sortKey : Float
}

class FibonacciHeap[T <: THasSortKey] {
	val intern = new com.bluemarsh.graphmaker.core.util.FibonacciHeap[T]

	def enqueue ( t : T ) : FibonacciHeap.Node[T] = intern.insert(t,t.sortKey)
	def dequeue() : T = intern.removeMin().asInstanceOf[T]
	def updateKey (node:FibonacciHeap.Node[T]) { intern.decreaseKey(node,node.data.asInstanceOf[T].sortKey) }
	def nonEmpty = ! intern.isEmpty
	def remove ( n : FibonacciHeap.Node[T] ) { intern.delete(n) }
	def clear () { intern.clear() }
	def peek : T = intern.min.data
}
object FibonacciHeap {
	type Node[T] = com.bluemarsh.graphmaker.core.util.FibonacciHeap.Node[T];
}