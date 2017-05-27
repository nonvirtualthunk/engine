package arx.core.vec

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import scalaxy.loops._

object VecFunc {
	/**
	  * Returns two vectors, the first representing the piecewise minimum, the second the piecewise maximum
	  */
	def minmax2f (vs : List[ReadVec2f]) : (ReadVec2f,ReadVec2f) = {
		val min = Vec2f(Float.MaxValue, Float.MaxValue)
		val max = Vec2f(Float.MinValue, Float.MinValue)

		for (v <- vs) {
			min.x = math.min(min.x,v.x)
			min.y = math.min(min.y,v.y)

			max.x = math.max(max.x,v.x)
			max.y = math.max(max.y,v.y)
		}

		(min,max)
	}

	/**
	  * Returns two vectors, the first representing the piecewise minimum, the second the piecewise maximum
	  */
	def minmax3f (vs : List[ReadVec3f]) : (ReadVec3f,ReadVec3f) = {
		val min = Vec3f(Float.MaxValue)
		val max = Vec3f(Float.MinValue)

		for (v <- vs) {
			min.x = math.min(min.x,v.x)
			min.y = math.min(min.y,v.y)
			min.z = math.min(min.z,v.z)

			max.x = math.max(max.x,v.x)
			max.y = math.max(max.y,v.y)
			max.z = math.max(max.z,v.z)
		}

		(min,max)
	}


	/**
	  * Returns two vectors, the first representing the piecewise minimum, the second the piecewise maximum
	  */
	def minmax2i (vs : List[ReadVec2i]) : (ReadVec2i,ReadVec2i) = {
		val min = Vec2i(Int.MaxValue, Int.MaxValue)
		val max = Vec2i(Int.MinValue, Int.MinValue)

		for (v <- vs) {
			min.x = math.min(min.x,v.x)
			min.y = math.min(min.y,v.y)

			max.x = math.max(max.x,v.x)
			max.y = math.max(max.y,v.y)
		}

		(min,max)
	}
}
