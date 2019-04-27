package arx.ai.search

import arx.core.{FibonacciHeap, THasSortKey}

import scala.collection.{TraversableLike, mutable}
import arx.Prelude._
import arx.core.metrics.Metrics
import arx.engine.lworld.LEntity

case class Pathfinder[T](name : String,
								 adjacencyFunction: T => Traversable[T],
								 costFunction: (LEntity, T, T) => Option[Float],
								 heuristicFunction: (T, T) => Float) {
	val timer = Metrics.timer(s"Pathfinder[$name].time")
	val examinedNodeHistogram = Metrics.histogram(s"Pathfinder[$name].nodesExamined")

	def findPathTo(entity : LEntity, from: T, to: T, maximumCost: Option[Float] = None): Option[Path[T]] = {
		findPathToAny(entity, from, to :: Nil, maximumCost)
	}
	def findPathToAny(entity : LEntity, from: T, toList: List[T], maximumCost: Option[Float] = None): Option[Path[T]] = {
		timer.timeStmt {
			var examined = 0

			val heuristicTarget = toList.head
			val to = toList.toSet

			val heap = new FibonacciHeap[SearchNode[T]]
			heap.enqueue(SearchNode(from, null, 0.0f, heuristicFunction(from, heuristicTarget)))

			val closedSet = new mutable.HashSet[T]
			val openSet = new java.util.HashMap[T, FibonacciHeap.Node[SearchNode[T]]]()

			val maxCost = maximumCost.getOrElse(Float.MaxValue)

			while (heap.nonEmpty && heap.peek.g <= maxCost) {
				val node = heap.dequeue()
				examined += 1

				if (to.contains(node.pos)) {
					examinedNodeHistogram.update(examined)
					return Some(node.path)
				} else {
					if (!closedSet.contains(node.pos)) {
						closedSet.add(node.pos)
						openSet.remove(node.pos)

						for (adjacency <- adjacencyFunction(node.pos)) {
							if (node.parent == null || adjacency != node.parent.pos) {
								costFunction(entity, node.pos, adjacency) match {
									case Some(cost) => {
										val newH = heuristicFunction(adjacency, heuristicTarget)
										val newNode = new SearchNode[T](adjacency, node, node.g + cost, newH)

										val existingResult = openSet.get(adjacency)
										if (existingResult == null || existingResult.data.g > newNode.g) {
											if (existingResult != null) {
												existingResult.data.g = newNode.g
												existingResult.data.parent = node
												heap.updateKey(existingResult)
											} else {
												val fibNode = heap.enqueue(newNode)
												openSet.put(adjacency, fibNode)
											}
										}
									}
									case None => // this indicates it cannot be moved into, so don't add it to anything
								}
							}
						}
					}
				}
			}

			examinedNodeHistogram.update(examined)
			None
		}
	}
}

protected[search] case class SearchNode[T](pos: T, var parent: SearchNode[T], var g: Float, h: Float) extends THasSortKey {
	def f = g + h

	override def sortKey: Float = f

	def path = Path(pathSteps.reverse)

	def pathSteps: List[PathStep[T]] = if (parent != null) {
		PathStep(pos, g - parent.g) :: parent.pathSteps
	} else {
		PathStep(pos, 0.0f) :: Nil
	}
}

case class Path[T](steps: List[PathStep[T]]) {
	def cost = steps.map(_.cost).sum
}

case class PathStep[T](node: T, cost: Float)