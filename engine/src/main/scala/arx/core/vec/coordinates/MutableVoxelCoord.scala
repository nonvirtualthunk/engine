package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/12
 * Time: 9:52 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.{ReadVec2i, ReadVec3i, Vec3i}

class MutableVoxelCoord(xa:Int,ya:Int,za:Int) extends Vec3i(xa,ya,za) with VoxelCoord {
	def this () { this(0,0,0) }

	def minWith ( v : ReadVec3i ) {
		x = math.min(x,v.x)
		y = math.min(y,v.y)
		z = math.min(z,v.z)
	}

	def maxWith ( v : ReadVec3i ) {
		x = math.max(x,v.x)
		y = math.max(y,v.y)
		z = math.max(z,v.z)
	}

	def set ( xar : Int , yar : Int , zar : Int ) {
		xi = xar
		yi = yar
		zi = zar
	}
}

object MutableVoxelCoord {
	def apply ( o : ReadVec3i ) = new MutableVoxelCoord(o.x,o.y,o.z)
	def apply ( x : Int, y : Int ,z : Int ) = new MutableVoxelCoord(x,y,z)
	def apply ( xy : ReadVec2i ,z : Int ) = new MutableVoxelCoord(xy.x,xy.y,z)
	def unapply ( v : MutableVoxelCoord ) : Option[(Int,Int,Int)] = Some((v.x,v.y,v.z))
}