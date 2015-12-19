package arx.core.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 7:11 AM
 */

import arx.Prelude._
import arx.core.datastructures.Voronoi.{Region, Edge}
import arx.core.math.Rectf
import arx.core.vec.{Vec2f, ReadVec2f}
import scala.collection.mutable.ListBuffer
import scalaxy.loops._

class Voronoi(points : List[ReadVec2f], min : ReadVec2f, max : ReadVec2f ) {
	lazy val regions = init()

	def init () = {
		val wrapped = new VoronoiGraph
		val cList = new cVertexList
		points.foreach(p => cList.InsertBeforeHead(new cVertex(p.x,p.y)))
		wrapped.Sort(cList)
		wrapped.generateVoronoi(min.x,max.x,min.y,max.y)


		val edgesByOriginator = Array.fill(points.size)(new ListBuffer[Edge])

		wrapped.resetIterator()
		var e = wrapped.getNext
		while (e != null) {
			val s1 = e.e.reg(0).sitenbr
			val s2 = e.e.reg(1).sitenbr
			if (e.x1 != e.x2 || e.y1 != e.y2) {
				val edge = Edge(Vec2f(e.x1,e.y1),Vec2f(e.x2,e.y2))
				edgesByOriginator(s1).append(edge)
				edgesByOriginator(s2).append(edge)
			}
			e = wrapped.getNext
		}

		edgesByOriginator.zipWithIndex.map{ case (l,i) => Region(l.toList,i) }.toList
	}

}

object Voronoi {
	def fromPoints (points : List[ReadVec2f], region : Rectf) = new Voronoi(points,region.min,region.max)

	case class Edge (start : ReadVec2f, end : ReadVec2f) {
		def p1 = start
		def p2 = end
		def points = List(start,end)
	}
	case class Region (edges : List[Edge], originatingIndex : Int)
}