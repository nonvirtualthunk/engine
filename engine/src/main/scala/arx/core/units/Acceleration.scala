package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:14 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.Vec3f

case class Acceleration ( var x : UnitOfAcceleration , var y : UnitOfAcceleration , var z : UnitOfAcceleration ) {
	def inMetersPerSecondSquared = Vec3f(x.inMetersPerSecondSquared,y.inMetersPerSecondSquared,z.inMetersPerSecondSquared)
	def inMetersPerSecondSquared (ret : Vec3f) = { ret.x = x.inMetersPerSecondSquared; ret.y = y.inMetersPerSecondSquared; ret.z = z.inMetersPerSecondSquared; ret }
	def - ( t : Acceleration ) : Acceleration = Acceleration(x - t.x,y - t.y,z - t.z)
	def + ( t : Acceleration ) : Acceleration = Acceleration(x + t.x,y + t.y,z + t.z)
	def * ( t : UnitOfTime ) : Velocity = new Velocity(x * t,y * t,z * t)
	def * ( t : UnitOfTimeSquared ) : Dimensions = new Dimensions(x * t,y * t,z * t)
	def * ( f : Float ) : Acceleration = new Acceleration(x * f,y * f,z * f)

	def apply ( i : Int ) = if ( i == 0 ) { x } else if ( i == 1 ) { y } else if ( i == 2 ) { z } else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	def update ( i : Int , s : UnitOfAcceleration ) {
		if ( i == 0 ) { x = s }
		else if ( i == 1 ) { y = s }
		else if ( i == 2 ) { z = s }
		else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	}

	def == ( v : Acceleration ) = v.x == x && v.y == y && v.z == z
	def != ( v : Acceleration ) = v.x != x || v.y != y || v.z != z

	def isNaN = x.overValue.value != x.overValue.value || y.overValue.value != y.overValue.value || z.overValue.value != z.overValue.value ||
					x.underValue.value != x.underValue.value || y.underValue.value != y.underValue.value || z.underValue.value != z.underValue.value
}