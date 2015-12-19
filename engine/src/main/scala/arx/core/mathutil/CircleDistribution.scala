package arx.core.mathutil

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/3/13
 * Time: 9:42 AM
 */

import arx.Prelude._


import scalaxy.loops._
import arx.core.vec.{ReadVec2f, Vec2f}

object CircleDistribution {
	/**
	 * Generates a point within a circle centered at the origin, with the given radius. Points
	 * generated in this manner should be uniformly distributed across the entire area of the
	 * circle.
	 *
	 * @param radius radius of the circle in which to generate a point
	 */
	def generatePoint (radius : Float) = {
		val t = 2.0f * pi * rand(0.0f,1.0f)
		val u = rand(0.0f,1.0f) + rand(0.0f,1.0f)
		val r = if ( u > 1.0f ) { 2.0f - u } else { u }

		ReadVec2f(cosf(t) * r * radius,sinf(t) * r * radius)
	}
}
