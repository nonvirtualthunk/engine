package arx.core.datastructures

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.algorithms.DelaunayTriangulation
import arx.core.math.Rectf
import arx.core.metrics.Metrics
import arx.core.vec.ReadVec2f
import org.scalatest.FlatSpec

import scala.util.Random
import scalaxy.loops._

class VoronoiTests extends FlatSpec {

	//	"Voronoi sorting" should "sort the list of points by y, with x as tiebreaker" in {
	//		val xs = Array(5.0f,2.0f,3.0f,9.0f,1.0f)
	//		val ys = Array(2.0f,1.0f,5.0f,4.0f,7.0f)
	//		val originators = fillArray(xs.length)(i => i)
	//		val tmpX = Array.ofDim[Float](5)
	//		val tmpY = Array.ofDim[Float](5)
	//		val tmpI = Array.ofDim[Int](5)
	//		VoronoiComputor.sortVertices(xs, ys, originators, tmpX, tmpY, tmpI, 0, xs.length-1)
	//
	//		require(xs.toList == List(2.0f, 5.0f, 9.0f, 3.0f, 1.0f))
	//		require(ys.toList == List(1.0f, 2.0f, 4.0f, 5.0f, 7.0f))
	//		require(originators.toList == List(1,0,3,2,4))
	//	}
	//

	"Quick timing" should "do thing" in {

		val rand = new Random()
		val points = fillList(200000)(_ => ReadVec2f(rand.nextFloat() * 1000.0f, rand.nextFloat() * 1000.0f))

		val timer = Metrics.timer("voronoi test")
		for (i <- 0 until 10 optimized) {
			timer.timeStmt {
				val vor = Voronoi.fromPoints(points, Rectf(0.0f, 0.0f, 1000.0f, 1000.0f))
				val allRegions = vor.regions
				println("Regions size: " + allRegions.size)
			}
		}

		Metrics.prettyPrintTimer(timer)
	}


	"Each point" should "appear three-ish times" in {
		val rand = new Random()
		val points = fillList(5000)(_ => ReadVec2f(rand.nextFloat() * 1000.0f, rand.nextFloat() * 1000.0f))

		val vor = Voronoi.fromPoints(points, Rectf(0.0f, 0.0f, 1000.0f, 1000.0f))
		val allRegions = vor.regions
		val cm = new CountMapi[ReadVec2f]()
		for (reg <- allRegions) {
			for (edge <- reg.edges) {
				for (i <- 0 until 2 optimized) {
					cm.increment((edge.point(i) * 10.0f).round, 1)
				}
			}
		}

		cm.intern.foreach(t => println(s"${t._1} : ${t._2}"))
	}
}
