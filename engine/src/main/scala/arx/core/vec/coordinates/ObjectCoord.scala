package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/12
 * Time: 10:27 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.traits.TSentinel
import arx.core.vec.{ReadVec2f, ReadVec3f}
import arx.core.{Moddable, SelfModdable}

trait ObjectCoord extends ReadVec3f with TMajorCoord with SelfModdable[ObjectCoord] {
	def toVoxelCoord = VoxelCoord(
		(floorf(x) + VoxelCoord.Center.x).toInt,
		(floorf(y) + VoxelCoord.Center.y).toInt,
		(floorf(z) + VoxelCoord.Center.z).toInt)
	def toVoxelCoord ( ret : MutableVoxelCoord ) = {
		ret.x = (x + VoxelCoord.Center.x).toInt
		ret.y = (y + VoxelCoord.Center.y).toInt
		ret.z = (z + VoxelCoord.Center.z).toInt
		ret
	}
	def toVoxelSpace = toVoxelCoord
	def toObjectCoord = this
	def toObjectCoordFoot = ObjectCoord(this.x,this.y,this.z)

	def scalarDistanceTo ( tc : TMajorCoord ) = ( this - tc.toObjectCoord ).lengthSafe
	def distanceTo ( tc : TMajorCoord ) = scalarDistanceTo(tc).voxels
	def euclidDistanceTo ( tc : TMajorCoord ) = {
		val c = tc.toObjectCoord
		((this.x-c.x) * (this.x-c.x) + (this.y-c.y) * (this.y-c.y) + (this.z-c.z) * (this.z-c.z)).voxels
	}

	override def + ( mv : Moddable[ReadVec3f] ) : ObjectCoord = { val v = mv.resolve(); new ConstObjectCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( mv : Moddable[ReadVec3f] ) : ObjectCoord = { val v = mv.resolve(); new ConstObjectCoord(x - v.x,y - v.y,z - v.z) }

	override def + ( v : ReadVec3f ) : ObjectCoord = { new MutableObjectCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( v : ReadVec3f ) : ObjectCoord = { new MutableObjectCoord(x - v.x,y - v.y,z - v.z) }
	override def / ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x/f,y/f,z/f) }
	override def * ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x*f,y*f,z*f) }

	def plusZ ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x,y,z + f) }
	def minusZ ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x,y,z - f) }
	def plusX ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x+f,y,z) }
	def minusX ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x-f,y,z) }
	def plusY ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x,y+f,z) }
	def minusY ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x,y-f,z) }
	def minusXY ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x-f,y-f,z) }
	def plusXY ( f : Float ) : ObjectCoord = { new MutableObjectCoord(x+f,y+f,z) }
	def plusXY ( dx : Float , dy : Float ) : ObjectCoord = { new MutableObjectCoord(x+dx,y+dy,z) }
	def plusXY ( v : ReadVec2f ) : ObjectCoord = { new MutableObjectCoord(x+v.x,y+v.y,z) }
	def plusAxis ( axis : Int, f : Float ) : ObjectCoord = {
		val v = new MutableObjectCoord(x,y,z)
		v(axis) += f
		v
	}

	def toStringer = "ObjectCoord(" + x + "," + y + "," + z + ")";

	override def baseValue = this
	override def resolve = this
}



object ObjectCoord {
	protected class sentinel extends ConstObjectCoord(0.0f,0.0f,0.0f) with TSentinel

	val Zero : ObjectCoord = zero
	object zero extends ConstObjectCoord(0.0f,0.0f,0.0f) {
		override def + ( mv : Moddable[ReadVec3f] ) : ObjectCoord = { ObjectCoord(mv.resolve()) }

		def + ( v : ObjectCoord ) : ObjectCoord = { v }
		def * ( v : ObjectCoord ) : ObjectCoord = { this }
		override def / ( f : Float ) : ObjectCoord = { this }
		override def * ( f : Float ) : ObjectCoord = { this }
	}
	val Sentinel : ObjectCoord = new sentinel

	def apply(v: ReadVec3f): ObjectCoord = new ConstObjectCoord(v.x,v.y,v.z)
	def apply(x : Float, y : Float,z : Float ) : ObjectCoord = new ConstObjectCoord(x,y,z)

	def toVoxelCoordZ(z: Float): Int = (floorf(z) + VoxelCoord.Center.z).toInt
	def fromVoxelCoordZ(z: Int): Float = (z - VoxelCoord.Center.z).toFloat + 0.5f

	def toVoxelCoordX(x: Float): Int = (floorf(x) + VoxelCoord.Center.x).toInt
	def fromVoxelCoordX(x: Int): Float = (x - VoxelCoord.Center.x).toFloat + 0.5f

	def toVoxelCoordY(y: Float): Int = (floorf(y) + VoxelCoord.Center.y).toInt
	def fromVoxelCoordY(y: Int): Float = (y - VoxelCoord.Center.y).toFloat + 0.5f
}
