package arx.core.math

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/12
 * Time: 9:22 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.vec.ReadVec3f

object Distance {
	val Epsilon = 0.00000001

	def minimumDistanceBetweenSegments (s1p0 : ReadVec3f,s1p1 : ReadVec3f,s2p0 : ReadVec3f,s2p1 : ReadVec3f) : Float =
	{
		minimumDistanceBetweenSegments(s1p0.x,s1p0.y,s1p0.z,s1p1.x,s1p1.y,s1p1.z,s2p0.x,s2p0.y,s2p0.z,s2p1.x,s2p1.y,s2p1.z)
	}

	def minimumVectorBetweenSegments(s1p0 : ReadVec3f,s1p1 : ReadVec3f,s2p0 : ReadVec3f,s2p1 : ReadVec3f) : ReadVec3f =
	{
		minimumVectorBetweenSegments(s1p0.x,s1p0.y,s1p0.z,s1p1.x,s1p1.y,s1p1.z,s2p0.x,s2p0.y,s2p0.z,s2p1.x,s2p1.y,s2p1.z)
	}

	def minimumVectorBetweenSegments (s1x0:Float,s1y0:Float,s1z0:Float,
													s1x1:Float,s1y1:Float,s1z1:Float,
													s2x0:Float,s2y0:Float,s2z0:Float,
													s2x1:Float,s2y1:Float,s2z1:Float ) : ReadVec3f = {
		val u = ReadVec3f(s1x1 - s1x0,s1y1 - s1y0,s1z1 - s1z0)
		val v = ReadVec3f(s2x1 - s2x0,s2y1 - s2y0,s2z1 - s2z0)
		val w = ReadVec3f(s1x0 - s2x0,s1y0 - s2y0,s1z0 - s2z0)
		val a = u.dot(u)
		val b = u.dot(v)
		val c = v.dot(v)
		val d = u.dot(w)
		val e = v.dot(w)
		val D = a*c - b*b
		var sc = 0.0f
		var sN = 0.0f
		var sD = D
		var tc = 0.0f
		var tN = 0.0f
		var tD = D

		if ( D < Epsilon ) {
			sN = 0.0f
			sD = 1.0f
			tN = e
			tD = c
		} else {
			sN = b*e - c*d
			tN = a*e - b*d
			if ( sN < 0.0f ) {
				sN = 0.0f
				tN = e
				tD = c
			} else if ( sN > sD ) {
				sN = sD
				tN = e + b
				tD = c
			}
		}

		if ( tN < 0.0f ) {
			tN = 0.0f
			if ( -d < 0.0f ) {
				sN = 0.0f
			} else if ( -d > a ) {
				sN = sD
			} else {
				sN = -d
				sD = a
			}
		} else if ( tN > tD ) {
			tN = tD
			if ((-d + b) < 0.0f) {
				sN = 0.0f
			} else if ((-d + b) > a ) {
				sN = sD
			} else {
				sN = (-d + b)
				sD = a
			}
		}

		sc = if (absf(sN) < Epsilon ) { 0.0f } else { sN / sD }
		tc = if (absf(tN) < Epsilon ) { 0.0f } else { tN / tD }

		w + (u * sc) - (v * tc)
	}
	
	def minimumDistanceBetweenSegments (s1x0:Float,s1y0:Float,s1z0:Float,
													s1x1:Float,s1y1:Float,s1z1:Float,
													s2x0:Float,s2y0:Float,s2z0:Float,
													s2x1:Float,s2y1:Float,s2z1:Float ) : Float = {
		val dP = minimumVectorBetweenSegments(s1x0,s1y0,s1z0,
															s1x1,s1y1,s1z1,
															s2x0,s2y0,s2z0,
															s2x1,s2y1,s2z1)
		dP.lengthSafe
	}
}
