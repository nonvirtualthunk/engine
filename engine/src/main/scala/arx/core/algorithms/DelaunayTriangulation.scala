package arx.core.algorithms

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.GrowableArray
import arx.core.datastructures.MultiMap
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f

import scalaxy.loops.rangeExtensions

object DelaunayTriangulation {

	def triangulate(points: Vector[ReadVec2f]) = {
		val context = new Context(points)
		context.triangulate()
	}

	protected class Context(val rawPoints: Vector[ReadVec2f]) {
		def triangulate(): Traversable[Triangle] = {
			if (rawPoints.size < 3) {
				Vector()
			} else {
				val maxCoordinate = rawPoints.toStream.map(p => p.x.max(p.y)).max
				val superScale = maxCoordinate * 48.0f

				val points = rawPoints ++: Vector(Vec2f(0.0f, superScale), Vec2f(superScale, 0.0f), Vec2f(-superScale, -superScale))
				val soup = new TriangleSoup(points)

				val superTriangle = Triangle(rawPoints.size, rawPoints.size + 1, rawPoints.size + 2)
				soup.add(superTriangle)
				soup.root = superTriangle


				for (i <- 0 until rawPoints.length optimized) {
					val point = rawPoints(i)
					soup.findContainingTriangle(point) match {
						case None =>
							val edge = soup.findNearestEdge(point)
							val first = soup.findOneTriangleSharing(edge).get
							val second = soup.findNeighbor(first, edge).get

							val firstNonEdgeVertex = first.getNonEdgeVertex(edge)
							val secondNonEdgeVertex = second.getNonEdgeVertex(edge)

							soup.remove(first)
							soup.remove(second)

							val triangle1 = Triangle(edge.p0, firstNonEdgeVertex, i)
							val triangle2 = Triangle(edge.p1, firstNonEdgeVertex, i)
							val triangle3 = Triangle(edge.p0, secondNonEdgeVertex, i)
							val triangle4 = Triangle(edge.p1, secondNonEdgeVertex, i)

							first.addDescendants(triangle1, triangle2, triangle3, triangle4)
							second.addDescendants(triangle1, triangle2, triangle3, triangle4)

							soup.add(triangle1)
							soup.add(triangle2)
							soup.add(triangle3)
							soup.add(triangle4)

							soup.legalizeEdge(triangle1, Edge(edge.p0, firstNonEdgeVertex), point, i)
							soup.legalizeEdge(triangle2, Edge(edge.p1, firstNonEdgeVertex), point, i)
							soup.legalizeEdge(triangle3, Edge(edge.p0, secondNonEdgeVertex), point, i)
							soup.legalizeEdge(triangle4, Edge(edge.p1, secondNonEdgeVertex), point, i)
						case Some(triangle) =>
							val a = triangle.p0
							val b = triangle.p1
							val c = triangle.p2

							soup.remove(triangle)

							val first = Triangle(a, b, i)
							val second = Triangle(b, c, i)
							val third = Triangle(c, a, i)

							triangle.addDescendants(first, second, third)

							soup.add(first)
							soup.add(second)
							soup.add(third)

							soup.legalizeEdge(first, Edge(a, b), point, i)
							soup.legalizeEdge(second, Edge(b, c), point, i)
							soup.legalizeEdge(third, Edge(c, a), point, i)
					}
				}

				for (i <- 0 until 3) {
					soup.removeTrianglesUsing(superTriangle.p(i))
				}
				soup.triangles
			}
		}

		class TriangleSoup(points: Vector[ReadVec2f]) {
			val triangles = new GrowableArray[Triangle]()
			var root: Triangle = null

			val trianglesByPoints = new MultiMap[Int, Triangle]

			def add(triangle: Triangle): Unit = {
				triangles.append(triangle)

				for (i <- 0 until 3 optimized) {
					trianglesByPoints.add(triangle.p(i), triangle)
				}
			}

			def remove(triangle: Triangle): Unit = {
				triangles.indexOf(triangle) match {
					case -1 => Noto.error("Attempted to remove non-existent triangle")
					case idx => swapAndPop(idx)
				}
			}

			def removeTrianglesUsing(pointIndex: Int): Unit = {
				var i = 0
				while (i < triangles.size) {
					val t = triangles(i)
					if (t.p0 == pointIndex || t.p1 == pointIndex || t.p2 == pointIndex) {
						swapAndPop(i)
					} else {
						i += 1
					}
				}
			}

			def swapAndPop(idx: Int): Unit = {
				val deleted = triangles(idx)
				triangles(idx) = triangles(triangles.size - 1)
				triangles.dropLast()

				deleted.removed = true
				for (i <- 0 until 3 optimized) {
					trianglesByPoints.remove(deleted.p(i), deleted)
				}
			}

			def findContainingTriangle(point: ReadVec2f): Option[Triangle] = {
				var triangle = root
				while (true) {
					if (triangle.descendantCount == 0) {
						if (triangle.removed) {
							return None
						} else {
							return Some(triangle)
						}
					} else {
						var i = 0
						while (i < triangle.descendantCount) {
							val t = triangle.descendants(i)
							if (contains(t, point)) {
								triangle = t
								i = 10000
							}
							i += 1
						}
					}
				}
				None
			}

			def recursiveFindContainingTriangle(triangle: Triangle, point: ReadVec2f): Option[Triangle] = {
				if (triangle.descendantCount == 0) {
					if (triangle.removed) {
						None
					} else {
						Some(triangle)
					}
				} else {
					for (i <- 0 until triangle.descendantCount optimized) {
						val t = triangle.descendants(i)
						if (contains(t, point)) {
							val res = recursiveFindContainingTriangle(t, point)
							if (res.isDefined) {
								return res
							}
						}
					}
					None
				}
			}

			def findNeighbor(srcT: Triangle, e: Edge): Option[Triangle] = {
				val p0matches = trianglesByPoints.get(e.p0)
				for (m <- p0matches) {
					if (m != srcT && (e.p1 == m.p0 || e.p1 == m.p1 || e.p1 == m.p2)) {
						return Some(m)
					}
				}

				val p1matches = trianglesByPoints.get(e.p1)
				for (m <- p1matches) {
					if (m != srcT && (e.p0 == m.p0 || e.p0 == m.p1 || e.p0 == m.p2)) {
						return Some(m)
					}
				}
				None
			}

			def findOneTriangleSharing(e: Edge) = {
				triangles.find(t => t.isNeighbor(e))
			}

			def findNearestEdge(p: ReadVec2f) = {
				var minDist = Float.MaxValue
				var minEdge: Edge = null
				for (t <- triangles) {
					for (i <- 0 until 3 optimized) {
						val ai = t.p(i)
						val bi = t.p(i) % 3
						val a = points(ai)
						val b = points(bi)
						val dist = distance(a, b)
						if (dist < minDist) {
							minDist = dist
							minEdge = Edge(ai, bi)
						}
					}
				}
				posit(minEdge != null, "attempted to find nearest edge with no triangles in soup")
				minEdge
			}

			def closestPoint(edgeP0: ReadVec2f, edgeP1: ReadVec2f, target: ReadVec2f) = {
				val ab = edgeP1 - edgeP0
				val tRaw = (target - edgeP0).dot(ab) / ab.dot(ab)
				val t = tRaw.max(0.0f).min(1.0f)
				edgeP0 + (ab * t)
			}

			def contains(triangle: Triangle, point: ReadVec2f): Boolean = {
				val a = points(triangle.p0)
				val b = points(triangle.p1)
				val c = points(triangle.p2)

				val pab = (point - a).cross(b - a)
				val pbc = (point - b).cross(c - b)
				if (!sameSign(pab, pbc)) return false
				val pca = (point - c).cross(a - c)
				if (!sameSign(pab, pca)) return false
				true
			}

			def legalizeEdge(triangle: Triangle, edge: Edge, newVertex: ReadVec2f, newVertexIndex: Int) {
				findNeighbor(triangle, edge) match {
					case Some(neighbourTriangle) =>
						if (isPointInCircumcircle(neighbourTriangle, newVertex)) {
							remove(triangle)
							remove(neighbourTriangle)
							val noneEdgeVertex = neighbourTriangle.getNonEdgeVertex(edge)
							val firstTriangle = Triangle(noneEdgeVertex, edge.p0, newVertexIndex)
							val secondTriangle = Triangle(noneEdgeVertex, edge.p1, newVertexIndex)
							triangle.addDescendants(firstTriangle, secondTriangle)
							neighbourTriangle.addDescendants(firstTriangle, secondTriangle)
							add(firstTriangle)
							add(secondTriangle)
							legalizeEdge(firstTriangle, Edge(noneEdgeVertex, edge.p0), newVertex, newVertexIndex)
							legalizeEdge(secondTriangle, Edge(noneEdgeVertex, edge.p1), newVertex, newVertexIndex)
						}
					case _ =>
				}
			}

			def isPointInCircumcircle(triangle: Triangle, point: ReadVec2f): Boolean = {
				val a = points(triangle.p0)
				val b = points(triangle.p1)
				val c = points(triangle.p2)

				val a11 = a.x - point.x
				val a21 = b.x - point.x
				val a31 = c.x - point.x
				val a12 = a.y - point.y
				val a22 = b.y - point.y
				val a32 = c.y - point.y
				val a13 = (a.x - point.x) * (a.x - point.x) + (a.y - point.y) * (a.y - point.y)
				val a23 = (b.x - point.x) * (b.x - point.x) + (b.y - point.y) * (b.y - point.y)
				val a33 = (c.x - point.x) * (c.x - point.x) + (c.y - point.y) * (c.y - point.y)
				val det = a11 * a22 * a33 + a12 * a23 * a31 + a13 * a21 * a32 - a13 * a22 * a31 - a12 * a21 * a33 - a11 * a23 * a32
				if (isOrientedCCW(a, b, c)) {
					det > 0.0d
				} else {
					det < 0.0d
				}
			}

			def isOrientedCCW(a: ReadVec2f, b: ReadVec2f, c: ReadVec2f): Boolean = {
				val a11 = a.x - c.x
				val a21 = b.x - c.x
				val a12 = a.y - c.y
				val a22 = b.y - c.y
				val det = a11 * a22 - a12 * a21
				det > 0.0d
			}
		}

	}

	case class Edge(p0: Int, p1: Int)

	case class Triangle(p0: Int, p1: Int, p2: Int) {
		def addDescendants(triangles: Triangle*): Unit = {
			for (triangle <- triangles) {
				addDescendant(triangle)
			}
		}

		def addDescendant(triangle: Triangle): Unit = {
			descendants(descendantCount) = triangle
			descendantCount += 1
		}

		protected[algorithms] val descendants = Array.ofDim[Triangle](4)
		protected[algorithms] var descendantCount = 0
		protected[algorithms] var removed = false

		def getNonEdgeVertex(edge: Edge) = {
			if (p0 != edge.p0 && p0 != edge.p1) {
				p0
			} else if (p1 != edge.p0 && p1 != edge.p1) {
				p1
			} else {
				p2
			}
		}

		def isNeighbor(e: Edge) =
			(e.p0 == p0 || e.p0 == p1 || e.p0 == p2) && (e.p1 == p0 || e.p1 == p1 || e.p1 == p2)

		def p(i: Int) = i match {
			case 0 => p0
			case 1 => p1
			case 2 => p2
			case _ => throw new IndexOutOfBoundsException(i + " is out of bounds for a triangle")
		}
	}

}
