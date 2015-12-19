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

case class Velocity ( var _x : UnitOfSpeed , var _y : UnitOfSpeed , var _z : UnitOfSpeed ) {
	def x = _x
	def y = _y
	def z = _z

	def x_= ( x_ : UnitOfSpeed ) { _x = x_ }
	def y_= ( y_ : UnitOfSpeed ) { _y = y_ }
	def z_= ( z_ : UnitOfSpeed ) { _z = z_ }

	def inMetersPerSecond = Vec3f(x.inMetersPerSecond,y.inMetersPerSecond,z.inMetersPerSecond)
	def inMetersPerSecond(ret : Vec3f) = { ret.x = x.inMetersPerSecond; ret.y = y.inMetersPerSecond; ret.z = z.inMetersPerSecond; ret }
	def inVoxelsPerSecond = Vec3f(x.inVoxelsPerSecond,y.inVoxelsPerSecond,z.inVoxelsPerSecond)
	def toBaseUnitOfMeasure = Vec3f(x.toBaseUnitOfMeasure,y.toBaseUnitOfMeasure,z.toBaseUnitOfMeasure)
	def - ( t : Velocity ) : Velocity = Velocity(x - t.x,y - t.y,z - t.z)
	def + ( t : Velocity ) : Velocity = Velocity(x + t.x,y + t.y,z + t.z)
	def * ( t : UnitOfTime ) : Dimensions = new Dimensions(x * t,y * t,z * t)
	def / ( t : UnitOfTime ) : Acceleration = Acceleration(x / t,y / t,z / t)

	def toVec3f = Vec3f(x.toBaseUnitOfMeasure,y.toBaseUnitOfMeasure,z.toBaseUnitOfMeasure)
	def getOrElse ( v : Velocity ) : Velocity = Velocity(x.getOrElse(v.x),y.getOrElse(v.y),z.getOrElse(v.z))
	def lengthSafe = {
		val xb = x.inMetersPerSecond
		val yb = y.inMetersPerSecond
		val zb = z.inMetersPerSecond
		val eb = xb*xb+yb*yb+zb*zb
		if ( eb != 0.0f ) {
			sqrtf(eb).m_s
		} else { zeroMetersPerSecond }
	}

	def apply ( i : Int ) = if ( i == 0 ) { x } else if ( i == 1 ) { y } else if ( i == 2 ) { z } else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	def update ( i : Int , s :UnitOfSpeed ) {
		if ( i == 0 ) { x = s }
		else if ( i == 1 ) { y = s }
		else if ( i == 2 ) { z = s }
		else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	}

	def == ( v : Velocity ) = v.x == x && v.y == y && v.z == z
	def != ( v : Velocity ) = v.x != x || v.y != y || v.z != z

	def isNaN = x.overValue.value != x.overValue.value || y.overValue.value != y.overValue.value || z.overValue.value != z.overValue.value ||
					x.underValue.value != x.underValue.value || y.underValue.value != y.underValue.value || z.underValue.value != z.underValue.value
	def isEmpty = false
	final def notEmpty = ! isEmpty

	def resolve() = this
	def baseValue() = this
}


object NoVelocity extends Velocity(NoSpeed,NoSpeed,NoSpeed) {
	override def isEmpty = true
	override def getOrElse ( v : Velocity ) = v

	override def x_= ( x_ : UnitOfSpeed ) { Noto.warn("Attempting to set x/y/z on NoVelocity") }
	override def y_= ( y_ : UnitOfSpeed ) { Noto.warn("Attempting to set x/y/z on NoVelocity") }
	override def z_= ( z_ : UnitOfSpeed ) { Noto.warn("Attempting to set x/y/z on NoVelocity") }
}
