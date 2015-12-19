package arx.core.mathutil

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/10/13
 * Time: 7:22 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.{ReadVec3f, Vec2f, Vec3f}

class RunningVector {
	val _min = Vec3f(Float.MaxValue)
	val _max = Vec3f(Float.MinValue)
	val _sum = Vec3f(0.0f)

	var count = 0

	def value ( x : Float , y : Float ) {
		_min.x = math.min(_min.x,x)
		_min.y = math.min(_min.y,y)
		
		_max.x = math.max(_max.x,x)
		_max.y = math.max(_max.y,y)

		_sum.x += x
		_sum.y += y

		count += 1
	}
	
	def value ( v : Vec2f ) {
		_min.x = math.min(_min.x,v.x)
		_min.y = math.min(_min.y,v.y)
		
		_max.x = math.max(_max.x,v.x)
		_max.y = math.max(_max.y,v.y)

		_sum.x += v.x
		_sum.y += v.y

		count += 1
	}
	
	def value ( v : Vec3f ) {
		_min.x = math.min(_min.x,v.x)
		_min.y = math.min(_min.y,v.y)
		_min.z = math.min(_min.z,v.z)
		
		_max.x = math.max(_max.x,v.x)
		_max.y = math.max(_max.y,v.y)
		_max.z = math.max(_max.z,v.z)

		_sum.x += v.x
		_sum.y += v.y
		_sum.z += v.z

		count += 1
	}

	def min : ReadVec3f = if ( count == 0 ) { Vec3f.Zero } else { _min }
	def max : ReadVec3f = if ( count == 0 ) { Vec3f.Zero } else { _max }
	def avg = if ( count == 0 ) { Vec3f.Zero } else { _sum / count.toFloat }
}