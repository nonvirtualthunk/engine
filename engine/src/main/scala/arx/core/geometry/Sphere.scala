package arx.core.geometry

import arx.core.math.Intersection
import arx.core.math.Intersection.LineIntersection
import arx.core.math.Intersection.NoLineIntersection
import arx.core.vec.{ReadVec3f, Vec3f}
import arx.core.traits.{TSentinel, TSentinelable}


/**
  * Created by IntelliJ IDEA.
  * User: nvt
  * Date: 10/2/11
  * Time: 5:06 PM
  * Created by nonvirtualthunk
  */

class Sphere(var center: ReadVec3f,var radius: Float) extends TIntersectable {
	def intersect ( start: ReadVec3f, end : ReadVec3f ) : LineIntersection = {
		Intersection.raySphereIntersection(start.x,start.y,start.z,end.x,end.y,end.z,center.x,center.y,center.z,radius)
		//		val L = (end - start).normalizeSafe
		//		val C = center - start
		//		val tmp = L.dot(C)
		//		val underRoot = (tmp * tmp) - C.dot(C) + (radius * radius)
		//
		//		if ( underRoot < 0.0f ){ List[Float]() }
		//		if ( math.abs(underRoot) < 0.000001f ){
		//			val mag = (end - start).lengthSafe
		//			if ( tmp < 0.0f || tmp > mag ) { List[Float]() }
		//			else{
		//				List[Float](tmp / mag)
		//			}
		//		}
		//		else {
		//			val mag = (end - start).lengthSafe
		//			val root = math.sqrt(underRoot)
		//			val d0 = tmp + root
		//			val d1 = tmp - root
		//			var ret = List[Float]()
		//			if ( d0 > 0.0f && d0 < mag ) {
		//				ret ::= (d0 / mag).toFloat
		//			}
		//			if ( d1 > 0.0f && d1 < mag ) {
		//				ret ::= (d1 / mag).toFloat
		//			}
		//			ret
		//		}
	}

	def minimumDistanceFromLineToCenter ( start: ReadVec3f, end : ReadVec3f ) : Float = {
		(((center - start).cross(center - end))).length / (end - start).length
	}

	def pointOnLineClosestToCenter ( start: ReadVec3f, end: ReadVec3f ) : Vec3f = {
		//		val u = ((center.x - start.x) * (end.x - start.x) + (center.y - start.y) * (end.y - start.y)) / length(end - start)
		val u = -1.0f * (start - center).dot(end - start) / math.pow((end - start).length,2).toFloat
		(start + (end - start) * u)
	}
}

trait TIntersectable extends TSentinelable {
	def intersect ( start : ReadVec3f , end : ReadVec3f ) : LineIntersection
}

object SentinelIntersectable extends TIntersectable with TSentinel{
	def intersect(start: ReadVec3f, end: ReadVec3f) = NoLineIntersection
}