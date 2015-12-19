package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:13 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._


import arx.application.Noto
import arx.core.vec.{ReadVec3f, Vec3f, Vec2f}
import arx.core.vec.coordinates.VoxelCoord

class Dimensions2(val x:UnitOfDistance,val y:UnitOfDistance) extends Serializable {
	def asVec = Vec2f(x.inMeters.toFloat,y.inMeters.toFloat)
	def x ( uod : UnitOfDistance ) : Dimensions = new Dimensions(x,y,uod)

	def area : UnitOfArea = x * y

	def + (t:UnitOfDistance) : Dimensions2 = new Dimensions2(x + t,y + t)
	def * (f:Float) : Dimensions2 = new Dimensions2(x * f,y * f)
	override def equals(p1: Any) = p1 match {
		case d : Dimensions2 => d.x == x && d.y == y
		case _ => false
	}

	def resolve() = this
	def baseValue() = this
}
@SerialVersionUID(1L)
class Dimensions(val x:UnitOfDistance,val y:UnitOfDistance,val z:UnitOfDistance) extends Serializable {
	def clampMin(m: UnitOfDistance) = new Dimensions( if ( x < m ) { m } else { x } , if ( y < m ) { m } else { y } , if ( z < m ) { m } else { z } )

	def asVec = Vec3f(x.inMeters.toFloat,y.inMeters.toFloat,z.inMeters.toFloat)
	def volume = new UnitOfVolume(Meter,x.inMeters*y.inMeters*z.inMeters)
	def surfaceArea = new UnitOfArea(Meter,x.inMeters*y.inMeters*2.0f + y.inMeters*z.inMeters*2.0f + x.inMeters*z.inMeters*2.0f)
	def inVoxels = Vec3f(x.inVoxels.toFloat,y.inVoxels.toFloat,z.inVoxels.toFloat)
	def inMeters = Vec3f(x.inMeters.toFloat,y.inMeters.toFloat,z.inMeters.toFloat)

	def - (t:UnitOfDistance) : Dimensions = new Dimensions(x - t,y - t,z - t)
	def + (t:UnitOfDistance) : Dimensions = new Dimensions(x + t,y + t,z + t)
	def * (t:Float) : Dimensions = new Dimensions(x * t,y * t,z * t)
	def * (t:Vec3f) : Dimensions = new Dimensions(x * t.x,y * t.y,z * t.z)
	def min : UnitOfDistance = scala.math.min( scala.math.min(x.inMeters,y.inMeters) , z.inMeters ).meters
	def max : UnitOfDistance = scala.math.max( scala.math.max(x.inMeters,y.inMeters) , z.inMeters ).meters
	def minArea : UnitOfArea = if ( x <= z && y <= z ) { x * y } else if ( x <= y && z <= y ) { x * z } else if ( y <= x && z <= x ) { y * z } else { throw new IllegalStateException }
	def maxArea : UnitOfArea = if ( x >= z && y >= z ) { x * y } else if ( x >= y && z >= y ) { x * z } else if ( y >= x && z >= x ) { y * z } else { throw new IllegalStateException }
	def scaleByVolume( f : Float ) = this * powf(f,0.33333f)
	override def toString = x + " x " + y + " x " + z
	override def equals(p1: Any) = p1 match {
		case d : Dimensions => d.x == x && d.y == y && d.z == z
		case _ => false
	}

	def toNearestVoxel = {
		new Dimensions((x.inVoxels.round-0.001f).voxels,(y.inVoxels.round-0.001f).voxels,(z.inVoxels.round-0.001f).voxels)
	}

	def resolve() = this
	def baseValue() = this

	def radius = {
		val xm = x.inMeters * 0.5f
		val ym = y.inMeters * 0.5f
		val zm = z.inMeters * 0.5f
		val euclid = xm*xm + ym*ym + zm*zm
		if ( euclid != 0 ) {
			sqrtf(euclid).meters
		} else {
			zeroMeters
		}
	}
	def apply(i : Int) = i match {
		case 0 => x
		case 1 => y
		case 2 => z
		case _ => Noto.severeError("Bad index into dimension"); x
	}
}
object Dimensions {
	def apply ( min : VoxelCoord , max : VoxelCoord ) = {
		new Dimensions( (max.x-min.x+1).voxels ,  (max.y - min.y+1).voxels , (max.z - min.z+1).voxels )
	}
	def fromRadius ( r : UnitOfDistance ) = {
		val nr = r * 1.6f
		new Dimensions(nr,nr,nr)
	}
	def fromVoxelV3 ( v : ReadVec3f ) = {
		new Dimensions( v.x.voxels , v.y.voxels , v.z.voxels )
	}
	val Zero = new Dimensions(zeroMeters,zeroMeters,zeroMeters)
}
class UnitlessDimensions2(x:Float,y:Float) extends Serializable{
	def x ( f : Float ) : UnitlessDimensions3 = new UnitlessDimensions3(x,y,f)
	def x ( uod : UnitOfDistance ) : Dimensions = {
		new Dimensions( uod.copyWithValue(x) , uod.copyWithValue(y) , uod )
	}
	def meters = new Dimensions2(x.meters,y.meters)
	def centimeters = new Dimensions2(x.centimeters,y.centimeters)
}
class UnitlessDimensions3(x:Float,y:Float,z:Float) extends Serializable {
	def meters : Dimensions = new Dimensions(x.meters,y.meters,z.meters)
	def meter = meters
	def centimeters : Dimensions = new Dimensions(x.centimeters,y.centimeters,z.centimeters)
	def in ( uom : UnitOfDistance ) : Dimensions = new Dimensions ( uom.copyWithValue(x) , uom.copyWithValue(y) , uom.copyWithValue(z) )
}
