package arx.core.datastructures

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 3/18/15
  * Time: 7:11 AM
  */

import arx.Prelude
import arx.core.datastructures.Voronoi.Edge
import arx.core.datastructures.Voronoi.Region
import arx.core.math.Rectf
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scalaxy.loops.rangeExtensions

class Voronoi(xValues: Array[Float], yValues : Array[Float], min: ReadVec2f, max: ReadVec2f) {
	lazy val regions = init()
	lazy val (points, neighbors) = pointsFromRegions(regions)

	protected def init() = {
		val wrapped = new VoronoiGraph

		wrapped.initializePoints(xValues, yValues)
		wrapped.generateVoronoi(min.x, max.x, min.y, max.y)

		val regions = Prelude.fillArray(xValues.length)(i => Region(Nil,i))
		val edgesByOriginator = Array.fill(xValues.length)(new ListBuffer[Edge])

		for (i <- 0 until wrapped.allEdges.size() optimized) {
			val graphEdge = wrapped.allEdges.get(i)
			// these are the two sites this wall is between
			val s1 = graphEdge.voronoiEdge.reg0.sitenbr
			val s2 = graphEdge.voronoiEdge.reg1.sitenbr

			// assuming this edge has a non-zero length, add it (do we want to omit zero length ones?)
			if (graphEdge.x1 != graphEdge.x2 || graphEdge.y1 != graphEdge.y2) {
				val regA = regions(s1)
				val regB = regions(s2)
				val edge = Edge(Vec2f(graphEdge.x1, graphEdge.y1), Vec2f(graphEdge.x2, graphEdge.y2), regA, regB)
				// this one edge is part of the
				edgesByOriginator(s1).append(edge)
				edgesByOriginator(s2).append(edge)
			}
		}

		edgesByOriginator.zipWithIndex.foreach { case (l,i) => regions(i).edges = l.toList }
		regions
	}

	protected def pointsFromRegions(regions : Array[Region]) = {
		val neighborsByPoint = new MultiMap[Int,Int]
		val bucket = VertexBucket(Rectf(min.x,min.y, max.x - min.x, max.y - min.y),2,5)

		for (reg <- regions) {
			for (edge <- reg.edges) {
				val p0idx = bucket.getOrAdd(edge.start, 0.0001f)
				val p1idx = bucket.getOrAdd(edge.end, 0.0001f)
				neighborsByPoint.add(p0idx, p1idx)
				neighborsByPoint.add(p1idx, p0idx)
			}
		}

		(bucket.allVertices,neighborsByPoint)
	}
}

object Voronoi {
	def fromPoints(points: List[ReadVec2f], region: Rectf) = {
		val xv = Array.ofDim[Float](points.size)
		val yv = Array.ofDim[Float](points.size)
		var index = 0
		for (point <- points) {
			xv(index) = point.x
			yv(index) = point.y
			index += 1
		}
		new Voronoi(xv, yv, region.min, region.max)
	}

	case class Edge(start: ReadVec2f, end: ReadVec2f, regionA : Region, regionB : Region) {
		def point(i : Int) = i match {
			case 0 => start
			case 1 => end
			case _ => throw new IndexOutOfBoundsException(s"Edge did not have an ${i} index point")
		}
		def otherRegion(than : Region) = if (than eq regionA) {
			regionB
		} else {
			regionA
		}
	}

	case class Region(var edges: List[Edge], originatingIndex: Int)
}


object VertexBucket {
	def apply(bounds : Rectf, nLayers : Int, nBuckets : Int) =
		new VertexBucket(new ArrayBuffer[ReadVec2f], bounds, nLayers, nBuckets)
}

class VertexBucket(val allVertices : ArrayBuffer[ReadVec2f], bounds : Rectf, nLayers : Int, nBuckets : Int) {
	var indices = new ArrayBuffer[Int]
	var subBuckets = if (nLayers > 0) {
		Prelude.fillArray(nBuckets, nBuckets)((i,j) => {
			val startX = bounds.minX + bounds.w * (i / nBuckets.toFloat)
			val startY = bounds.minY + bounds.h * (j / nBuckets.toFloat)
			val w = bounds.w * (1.0f / nBuckets.toFloat)
			val h = bounds.h * (1.0f / nBuckets.toFloat)
			new VertexBucket(allVertices, Rectf(startX,startY,w,h),nLayers-1,nBuckets)
		})
	} else {
		null
	}

	@tailrec
	final def getOrAdd(v : ReadVec2f, minDist : Float) : Int = {
		nLayers match {
			case 0 =>
				indices.find(i => Prelude.distance(v, allVertices(i)) < minDist) match {
					case Some(idx) => idx
					case None =>
						val idx = allVertices.size
						allVertices.append(v)
						indices.append(idx)
						idx
				}
			case _ =>
				val xBucket = (((v.x - bounds.x) / bounds.w) * nBuckets).toInt.min(nBuckets-1).max(0)
				val yBucket = (((v.y - bounds.y) / bounds.h) * nBuckets).toInt.min(nBuckets-1).max(0)
				subBuckets(xBucket)(yBucket).getOrAdd(v,minDist)
		}
	}

	@tailrec
	final def contains(v : ReadVec2f, minDist : Float) : Option[Int] = {
		nLayers match {
			case 0 =>
				indices.find(i => Prelude.distance(v, allVertices(i)) < minDist)
			case _ =>
				val xBucket = (((v.x - bounds.x) / bounds.w) * nBuckets).toInt.min(nBuckets-1).max(0)
				val yBucket = (((v.y - bounds.y) / bounds.h) * nBuckets).toInt.min(nBuckets-1).max(0)
				subBuckets(xBucket)(yBucket).contains(v,minDist)
		}
	}
//
//	@tailrec
//	private final def add(v : ReadVec2f, idx : Int): Unit = {
//		nLayers match {
//			case 0 =>
//				indices.append(idx)
//			case _ =>
//				val xBucket = (((v.x - bounds.x) / bounds.w) * nBuckets).toInt
//				val yBucket = (((v.y - bounds.y) / bounds.h) * nBuckets).toInt
//				subBuckets(xBucket)(yBucket).add(v,idx)
//		}
//	}
}

///*
//* The author of this software is Steven Fortune.  Copyright (c) 1994 by AT&T
//* Bell Laboratories.
//* Permission to use, copy, modify, and distribute this software for any
//* purpose without fee is hereby granted, provided that this entire notice
//* is included in all copies of any software which is or includes a copy
//* or modification of this software and in all copies of the supporting
//* documentation for such software.
//* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
//* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
//* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
//* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
//*/
//
///*
//* This code was originally written by Stephan Fortune in C code.  I, Shane O'Sullivan,
//* have since modified it, encapsulating it in a C++ class and, fixing memory leaks and
//* adding accessors to the Voronoi Edges.
//* Permission to use, copy, modify, and distribute this software for any
//* purpose without fee is hereby granted, provided that this entire notice
//* is included in all copies of any software which is or includes a copy
//* or modification of this software and in all copies of the supporting
//* documentation for such software.
//* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
//* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
//* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
//* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
//*/
//
///*
//* Java Version by Zhenyu Pan
//* Permission to use, copy, modify, and distribute this software for any
//* purpose without fee is hereby granted, provided that this entire notice
//* is included in all copies of any software which is or includes a copy
//* or modification of this software and in all copies of the supporting
//* documentation for such software.
//* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
//* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
//* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
//* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
//*/
//
///*
// * Scala Version by Samuel Bock
// * This is actually a significant adaptation/rewrite from the original, but
// * maintains the core functionality.
// *
// * Permission to use, copy, modify, and distribute this software for any
//* purpose without fee is hereby granted, provided that this entire notice
//* is included in all copies of any software which is or includes a copy
//* or modification of this software and in all copies of the supporting
//* documentation for such software.
//* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
//* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
//* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
//* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
// */
//
//object VoronoiComputor {
//
//	//
//	//	def computeMinMax(xs: Array[Float], ys: Array[Float]) = {
//	//		var minX = Float.MaxValue
//	//		var maxX = Float.MinValue
//	//		var minY = Float.MaxValue
//	//		var maxY = Float.MinValue
//	//
//	//		var i = 0
//	//		while (i < xs.length) {
//	//			minX = math.min(minX, xs(i))
//	//			maxX = math.max(maxX, xs(i))
//	//			minY = math.min(minY, ys(i))
//	//			maxY = math.max(maxY, ys(i))
//	//			i += 1
//	//		}
//	//		(minX, maxX, minY, maxY)
//	//	}
//	//
//
//	/**
//	  * x,y arrays, lower bound (inclusive), upper bound (inclusive)
//	  */
//	def sortVertices(xs: Array[Float], ys: Array[Float], originators: Array[Int], tmpX: Array[Float], tmpY: Array[Float], tmpI: Array[Int], lb: Int, ub: Int): Unit = {
//		if (lb < ub) {
//			val dividingPoint = (lb + ub) >> 1
//			sortVertices(xs, ys, originators, tmpX, tmpY, tmpI, lb, dividingPoint)
//			sortVertices(xs, ys, originators, tmpX, tmpY, tmpI, dividingPoint + 1, ub)
//
//			var li = lb
//			var ri = dividingPoint + 1
//			var ti = 0
//			while (li <= dividingPoint && ri <= ub) {
//				val lx = xs(li)
//				val rx = xs(ri)
//				val ly = ys(li)
//				val ry = ys(ri)
//
//				if (ly < ry || (ly == ry && lx < rx)) {
//					tmpX(ti) = lx
//					tmpY(ti) = ly
//					tmpI(ti) = originators(li)
//					li += 1
//				} else {
//					tmpX(ti) = rx
//					tmpY(ti) = ry
//					tmpI(ti) = originators(ri)
//					ri += 1
//				}
//				ti += 1
//			}
//
//			while (li <= dividingPoint) {
//				tmpX(ti) = xs(li)
//				tmpY(ti) = ys(li)
//				tmpI(ti) = originators(li)
//				li += 1
//				ti += 1
//			}
//			while (ri <= ub) {
//				tmpX(ti) = xs(ri)
//				tmpY(ti) = ys(ri)
//				tmpI(ti) = originators(ri)
//				ri += 1
//				ti += 1
//			}
//
//			Array.copy(tmpX, 0, xs, lb, ub - lb + 1)
//			Array.copy(tmpY, 0, ys, lb, ub - lb + 1)
//			Array.copy(tmpI, 0, originators, lb, ub - lb + 1)
//		}
//	}
//
//	def sortVertices(xs: Array[Float], ys: Array[Float]) = {
//		val nPoints = xs.length
//		val originators = Array.ofDim[Int](nPoints)
//		var i = 0
//		while (i < nPoints) {
//			originators(i) = i
//			i += 1
//		}
//
//		val tmpX = Array.ofDim[Float](nPoints)
//		val tmpY = Array.ofDim[Float](nPoints)
//		val tmpI = Array.ofDim[Int](nPoints)
//
//		sortVertices(xs, ys, originators, tmpX, tmpY, tmpI, 0, nPoints - 1)
//		originators
//	}
//
//	class VoronoiEdge {
//		var a = 0.0f
//		var b = 0.0f
//		var c = 0.0f
//
//		var ep0 = 0
//		var ep1 = 0
//
//		var reg0 = 0
//		var reg1 = 0
//
//		var edgenbr = 0
//	}
//
//	class HalfEdge {
//		var left: HalfEdge = _
//		var right: HalfEdge = _
//		var edge: VoronoiEdge = _
//		var deleted = false
//		var pm = 0
//		var vertexX = 0.0f
//		var vertexY = 0.0f
//		// vertex
//		var ystar = 0.0f
//		var next: HalfEdge = _
//	}
//
//	object HalfEdge {
//		def apply(e: VoronoiEdge, pm: Int) = {
//			val ret = new HalfEdge
//			ret.edge = e
//			ret.pm = pm
//			ret
//		}
//	}
//
//	class PointMut {
//		var x : Float = 0.0f
//		var y : Float = 0.0f
//	}
//
//	class SiteIter(var index : Int, xs : Array[Float], ys : Array[Float], originators : Array[Int]) {
//		def x = xs(index)
//		def y = ys(index)
//		def originator = originators(index)
//	}
//
//	class VoronoiContext(xs: Array[Float], ys: Array[Float]) {
//		assert(xs.length == ys.length)
//
//		val originators = sortVertices(xs, ys)
//
//		val nPoints = xs.length
//		val sqrtNSites = math.sqrt(nPoints + 4).toInt // why +4? No idea
//
//		val PQ = new PQ(nPoints)
//		val EQ = new EQ(sqrtNSites)
//
//		val xmin = xs(0)
//		val ymin = ys(0)
//		val xmax = xs(xs.length - 1)
//		val ymax = ys(ys.length - 1)
//		val deltax = xmax - xmin
//		val deltay = ymax - ymin
//
//		class PQ(nPoints : Int) {
//			val sqrtNSites = math.sqrt(nPoints + 4).toInt // why +4? No idea
//
//			var count = 0
//			var minI = 0
//			val hashsize = 4 * sqrtNSites
//			val hash = Array.fill(hashsize)(new HalfEdge)
//
//			def isEmpty() = count == 0
//			def min(res : PointMut) {
//				var i = -1
//				while (hash(minI).next == null) {
//					minI += 1
//				}
//				res.x = hash(minI).next.vertexX
//				res.y = hash(minI).next.ystar
//			}
//
//			def extractMin() = {
//				val curr = hash(minI).next
//				hash(minI).next = curr.next
//				count -= 1
//				curr
//			}
//		}
//
//		class EQ(sqrtNSites: Int) {
//			val hashSize = 2 * sqrtNSites
//			val hash = Array.ofDim[HalfEdge](hashSize)
//			val leftEnd = HalfEdge(null, 0)
//			val rightEnd = HalfEdge(null, 0)
//			leftEnd.left = null
//			leftEnd.right = rightEnd
//			rightEnd.left = leftEnd
//			rightEnd.right = null
//			hash(0) = leftEnd
//			hash(hash.length-1) = rightEnd
//
//			val le = 0
//			val re = 1
//
//			def getHash(b : Int) : HalfEdge = {
//				if (b < 0 || b >= hashSize) {
//					return null
//				}
//				val he = hash(b)
//				if (he == null || !he.deleted) {
//					return he
//				}
//
//				hash(b) = null
//				null
//			}
//
//			def rightOf(el: HalfEdge, x: Float, y: Float) = {
//				val e = el.edge
//				val topsiteX = xs(e.reg1)
//				val topsiteY = ys(e.reg1)
//				var rightOfSite = false
//
//				if (x > topsiteX) {
//					rightOfSite = true
//				}
//				if (rightOfSite && el.pm == le) {
//					true
//				} else if (!rightOfSite && el.pm == re) {
//					false
//				} else {
//					var above = false
//
//					if (e.a == 1.0f) {
//						val dyp = y - topsiteY
//						val dxp = x - topsiteX
//						var fast = false
//						if ((! rightOfSite & (e.b < 0.0f)) | (rightOfSite & (e.b >= 0.0f))) {
//							above = dyp >= e.b * dxp
//							fast = above
//						} else {
//							above = x + y * e.b > e.c
//							if (e.b < 0.0f) {
//								above = !above
//							}
//							if (!above) {
//								fast = true
//							}
//						}
//
//						if (!fast) {
//							val dxs = topsiteX - xs(e.reg0)
//							above = e.b * (dxp * dxp - dyp * dyp) < dxs * dyp * (1.0 + 2.0 * dxp / dxs + e.b * e.b)
//							if (e.b < 0.0f) {
//								above = !above
//							}
//						}
//					} else {
//						val yl = e.c - e.a * x
//						val t1 = y - yl
//						val t2 = x - topsiteX
//						val t3 = yl - topsiteY
//						above = t1 * t1 > t2 * t2 + t3 * t3
//					}
//					if (el.pm == le) {
//						above
//					}  else {
//						!above
//					}
//				}
//
//			}
//
//			def leftbnd(x: Float, y: Float) = {
//				var i = 0
//
//				/* Use hash table to get close to desired halfedge */
//				// use the hash function to find the place in the hash map that this
//				// HalfEdge should be
//				var bucket = ((x - xmin) / deltax * hashSize).toInt
//
//				bucket = bucket.max(0)
//				bucket = bucket.min(hashSize)
//
//				var he = getHash(bucket)
//				if (he == null) {
//					var i = 1
//					while (i < hashSize && he == null) {
//						he = getHash(bucket - i)
//						if (he == null) {
//							he = getHash(bucket + i)
//						}
//						i += 1
//					}
//				}
//
//				if (he == leftEnd || (he != rightEnd && rightOf(he, x, y)))
//			}
//		}
//
//		def generate(): Unit = {
//			var lbnd : HalfEdge = null
//
//			val newIntStar = new PointMut
//			var newSite = new SiteIter(0,xs,ys,originators)
//			while (true) {
//				if (! PQ.isEmpty()) {
//					PQ.min(newIntStar)
//				}
//				if (newSite != null &&
//					(PQ.isEmpty() || newSite.y < newIntStar.y ||
//						(newSite.y == newIntStar.y && newSite.x < newIntStar.x))) {
//					/* new site is smallest -this is a site event */
//					// get the first HalfEdge to the LEFT of the new site
//					lbnd = ELleftbnd(newSite.x,newSite.y)
//				}
//			}
//		}
//	}
//
//	/**
//	  * Generates a voronoi diagram based on the input base points. The arrays may be rearranged, ownership is
//	  * effectively transferred to the function when this is called
//	  */
//	def generateVoronoi(xs: Array[Float], ys: Array[Float]) = {
//		val ctx = new VoronoiContext(xs,ys)
//		ctx.generate()
//	}
//}