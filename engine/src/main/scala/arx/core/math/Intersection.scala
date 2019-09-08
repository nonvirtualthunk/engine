package arx.core.math

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/12
 * Time: 9:10 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.traits.{TSentinel, TSentinelable}
import arx.core.vec.{ReadVec3f, Vec3f}

object Intersection {
	val Epsilon = 0.00001f

	class LineIntersection extends TSentinelable {
		var _t0 : Float = Float.NaN
		def t0 = _t0
		def t0_= ( f : Float ) { _t0 = f }
		var _t1 : Float = Float.NaN
		def t1 = _t1
		def t1_= ( f : Float ) { _t1 = f }

		def head = t0
		def isEmpty = t0 != t0
		def nonEmpty = t0 == t0

		def apply ( i : Int ) = if ( i == 0 ) { t0 } else { t1 }
		def update ( i : Int , f : Float ) { if ( i == 0 ) { t0 = f } else { t1 = f } }

		def numIntersections = if ( t0 != t0 ) { 0 } else if ( t1 != t1 ) { 1 } else { 2 }

		def intersections = if (t0.isNaN) {
			Nil
		} else if (t1.isNaN) {
			List(t0)
		} else {
			List(t0, t1)
		}

		override def toString = numIntersections match {
			case 0 => "(No Intersection)"
			case 1 => "(" + t0 + ")"
			case 2 => "(" + t0 + "," + t1 + ")"
			case _ => "WAT"
		}
	}
	object NoLineIntersection extends LineIntersection with TSentinel{
		override def t0 = Float.NaN
		override def t1 = Float.NaN
		override def isEmpty = true
		override def nonEmpty = false
		override def numIntersections = 0
	}

	def rayCapsuleIntersection ( rayX1 : Float , rayY1 : Float , rayZ1 : Float ,
										  rayX2 : Float , rayY2 : Float , rayZ2 : Float ,
										  capsuleCenterX : Float , capsuleCenterY : Float , capsuleCenterZ : Float,
										  capsuleHeight : Float , capsuleRadius : Float ) =
	{
		val minDist = Distance.minimumDistanceBetweenSegments(
			capsuleCenterX,capsuleCenterY,capsuleCenterZ-capsuleHeight,
			capsuleCenterX,capsuleCenterY,capsuleCenterZ+capsuleHeight,
			rayX1,rayY1,rayZ1,
			rayX2,rayY2,rayZ2
		)

		if ( minDist <= capsuleRadius ) {

		} else {

		}
	}


	def raySphereIntersection ( ray1 : ReadVec3f , ray2 : ReadVec3f , sphereCenter : ReadVec3f , radius : Float ) : LineIntersection = {
		raySphereIntersection(ray1.x,ray1.y,ray1.z,ray2.x,ray2.y,ray2.z,sphereCenter.x,sphereCenter.y,sphereCenter.z,radius)
	}

	def raySphereIntersection ( rayX1 : Float , rayY1 : Float , rayZ1 : Float ,
										  rayX2 : Float , rayY2 : Float , rayZ2 : Float ,
										  sphereCenterX : Float , sphereCenterY : Float , sphereCenterZ : Float,
										  radius : Float ) : LineIntersection =
	{
		val ret = new LineIntersection

		val rayDelta = Vec3f(rayX2 - rayX1,rayY2 - rayY1,rayZ2 - rayZ1)
		val L = rayDelta.normalizeSafe
		val C = Vec3f(sphereCenterX - rayX1,sphereCenterY - rayY1,sphereCenterZ - rayZ1)
		val tmp = L.dot(C)
		val underRoot = (tmp * tmp) - C.dot(C) + (radius * radius)

		if ( underRoot < 0.0f ){ List[Float]() }
		if ( math.abs(underRoot) < 0.000001f ){
			val mag = rayDelta.lengthSafe
			if ( tmp < 0.0f || tmp > mag ) {}
			else{
				ret(0) = (tmp / mag)
			}
		}
		else {
			val mag = rayDelta.lengthSafe
			val root = math.sqrt(underRoot)
			val d0 = tmp + root
			val d1 = tmp - root
			var i = 0
			if ( d0 > 0.0f && d0 < mag ) {
				ret(i) = (d0 / mag).toFloat
				i += 1
			}
			if ( d1 > 0.0f && d1 < mag ) {
				ret(i) = (d1 / mag).toFloat
				i += 1
			}
		}

		ret
	}

	def rayCylinderIntersection ( 	ray1 : ReadVec3f ,
												ray2 : ReadVec3f ,
												cylinder1 : ReadVec3f ,
												cylinder2 : ReadVec3f,
												radius : Float ) : LineIntersection =
		{
			rayCylinderIntersection(ray1.x,ray1.y,ray1.z,ray2.x,ray2.y,ray2.z,cylinder1.x,cylinder1.y,cylinder1.z,cylinder2.x,cylinder2.y,cylinder2.z,radius)
		}

	def rayCylinderIntersection ( 	rayX1 : Float , rayY1 : Float , rayZ1 : Float ,
											  rayX2 : Float , rayY2 : Float , rayZ2 : Float ,
											  cylinderX1 : Float , cylinderY1 : Float , cylinderZ1 : Float,
											  cylinderX2 : Float , cylinderY2 : Float , cylinderZ2 : Float,
											  radius : Float ) : LineIntersection =
	{
		val ret = new LineIntersection

		val r = radius
		val d = Vec3f(cylinderX2 - cylinderX1,cylinderY2 - cylinderY1,cylinderZ2 - cylinderZ1)
		val m = Vec3f(rayX1 - cylinderX1,rayY1 - cylinderY1,rayZ1 - cylinderZ1)
		val n = Vec3f(rayX2 - rayX1,rayY2 - rayY1,rayZ2 - rayZ1)

		val md = m dot d
		val nd = n dot d
		val dd = d dot d

		if ( md < 0.0f && md + nd < 0.0f ) {}
		else if ( md > dd && md + nd > dd ) {}
		else {
			val nn = n dot n
			val mn = m dot n
			val a = dd * nn - nd * nd
			val k = (m dot m) - r * r
			val c = dd * k - md * md
			if ( absf(a) < Epsilon ) {
				if ( c > 0.0f ) {}
				else {
					if ( md < 0.0f ) { ret(0) = -mn / nn }
					else if ( md > dd ) { ret(0) = (nd - mn) / nn }
					else { ret(0) = 0.0f }
				}
			} else {
				val b = dd * mn - nd * md
				val discr = b * b - a * c
				if ( discr < 0.0f ) {}
				else{
					val t = (-b - sqrtf(discr)) / a
					if ( t < 0.0f || t > 1.0f ) {}
					else {
						if ( md + t * nd < 0.0f ) {
							if ( nd <= 0.0f ) {}
							else {
								val t2 = -md / nd
								if ( k + 2.0f * t2 * (mn + t2 * nn) <= 0.0f ) {
									ret(0) = t2
								}
							}
						} else if ( md + t * nd > dd ) {
							if ( nd >= 0.0f ) {}
							else {
								val t2 = (dd - md) / nd
								if ( k + dd - 2.0f * md + t2 * (2.0f * (mn - nd) + t2 * nn) <= 0.0f ) {
									ret(0) = t2
								}
							}
						} else {
							ret(0) = t
						}
					}
				}
			}
		}

		ret
	}

	def rayTriangleIntersection(start : ReadVec3f, end : ReadVec3f, p0 : ReadVec3f, p1 : ReadVec3f, p2 : ReadVec3f) : LineIntersection = {
		val edge1 = p1 - p0
		val edge2 = p2 - p0

		val rayVec = (end - start).normalizeSafe
		val h = rayVec.cross(edge2)
		val a = edge1.dot(h)

		if (a > -0.000001f && a < 0.000001f) {
			NoLineIntersection
		} else {
			val f = 1.0f / a
			val s = start - p0

			val u = f * s.dot(h)
			if (u < 0.0f || u > 1.0f) {
				return NoLineIntersection
			}
			val q = s.cross(edge1)
			val v = f * rayVec.dot(q)
			if (v < 0.0f || u + v > 1.0f) {
				return NoLineIntersection
			}

			val t = f * edge2.dot(q)
			if (t > 0.000001f) {
				val li = new LineIntersection
				li.t0 = t
				li
			} else {
				NoLineIntersection
			}
		}

	}

	@inline
	def coneSphereInclusionTest ( 	normal : ReadVec3f ,
												coneCos : Float , coneTan : Float ,
											 	sphere : ReadVec3f,
												sphereRadius : Float ) : Int =
	{
		coneSphereInclusionTest(normal.x,normal.y,normal.z,coneCos,coneTan,sphere.x,sphere.y,sphere.z,sphereRadius)
	}

	@inline
	def coneSphereInclusionTest ( 	normalX : Float , normalY : Float , normalZ : Float ,
												coneCos : Float , coneTan : Float ,
											 	sphereX : Float , sphereY : Float , sphereZ : Float,
												sphereRadius : Float ) : Int =
	{
		val a = sphereX * normalX + sphereY * normalY + sphereZ * normalZ
		val b = a * coneTan
		val VdotV = (sphereX * sphereX + sphereY * sphereY + sphereZ * sphereZ)
		val c = sqrtf( VdotV - a*a)
		val d = c - b
		val e = d * coneCos

		if ( e >= sphereRadius ) { 0 }
		else if ( e <= -sphereRadius ) { 1 }
		else { -1 }

		/*
		V = sphere.center - cone.apex_location
		a = V * cone.direction_normal
		b = a * cone.tan
		c = sqrt( V*V - a*a )
		d = c - b
		e = d * cone.cos

		now  if ( e >= sphere.radius ) , cull the sphere
		else if ( e <=-sphere.radius ) , totally include the sphere
		else the sphere is partially included.
		 */
	}
}