package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/12
 * Time: 10:28 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec._
import arx.Prelude._
import Cardinals._

trait VoxelCoord extends ReadVec3i with TMajorCoord {
	def toObjectCoord = ObjectCoord(	(x - VoxelCoord.Center.x).toFloat + 0.5f,
												(y - VoxelCoord.Center.y).toFloat + 0.5f,
												(z - VoxelCoord.Center.z).toFloat + 0.5f)
	def toObjectCoordFoot = ObjectCoord((x - VoxelCoord.Center.x).toFloat + 0.5f,
													(y - VoxelCoord.Center.y).toFloat + 0.5f,
													(z - VoxelCoord.Center.z).toFloat)
	def toObjectSpace = toObjectCoord
	def toVoxelCoord = this

	override def + ( v : ReadVec3i ) : MutableVoxelCoord = { new MutableVoxelCoord(x + v.x,y + v.y,z + v.z) }
	override def - ( v : ReadVec3i ) : MutableVoxelCoord = { new MutableVoxelCoord(x - v.x,y - v.y,z - v.z) }
	override def << ( s : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x << s, y << s, z << s) }
	override def >> ( s : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x >> s, y >> s, z >> s) }

	def + ( v : (Int,Int,Int) ) : MutableVoxelCoord = { new MutableVoxelCoord(x + v._1,y + v._2,z + v._3) }
	def - ( v : (Int,Int,Int) ) : MutableVoxelCoord = { new MutableVoxelCoord(x - v._1,y - v._2,z - v._3) }
	override def / ( v : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x / v, y / v, z / v) }
	override def - ( v : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x - v, y - v, z - v) }
	override def + ( v : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x + v, y + v, z + v) }
	def min ( v : VoxelCoord ) : VoxelCoord = VoxelCoord(v.x.min(x),v.y.min(y),v.z.min(z))
	def max ( v : VoxelCoord ) : VoxelCoord = VoxelCoord(v.x.max(x),v.y.max(y),v.z.max(z))

	def plusZ ( f : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x,y,z + f) }
	def minusZ ( f : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x,y,z - f) }
	def plusX ( f : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x+f,y,z) }
	def minusX ( f : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x-f,y,z) }
	def plusY ( f : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x,y+f,z) }
	def minusY ( f : Int ) : MutableVoxelCoord = { new MutableVoxelCoord(x,y-f,z) }
	def plusAxis (axis : Int, f : Int) : VoxelCoord  = axis match {
		case 0 => plusX(f)
		case 1 => plusY(f)
		case 2 => plusZ(f)
	}
	def plusXYZ ( xf : Int , yf : Int , zf : Int ) : MutableVoxelCoord = {
		new MutableVoxelCoord(x + xf,y + yf,z + zf)
	}


	def > ( i : Int ) : Boolean = { x > i && y > i && z > i }
	def >= ( i : Int ) : Boolean = { x >= i && y >= i && z >= i }
	def >= ( v : VoxelCoord ) : Boolean = { x >= v.x && y >= v.y && z >= v.z }
	def <= ( v : VoxelCoord ) : Boolean = { x <= v.x && y <= v.y && z <= v.z }
	def < ( v : ReadVec3i ) : Boolean = x < v.x && y < v.y && z < v.z

	def distanceInVoxelsTo (v : VoxelCoord) = {
		val dx = this.x - v.x
		val dy = this.y - v.y
		val dz = this.z - v.z
		val d = dx*dx + dy*dy + dz*dz
		if (d == 0) {
			0.0f
		} else {
			math.sqrt(d).toFloat
		}
	}
	def distanceTo ( tc : TMajorCoord ) = {
		val diff = ( this - tc.toVoxelCoord )
		val euclidean = diff.x * diff.x + diff.y * diff.y + diff.z * diff.z
		if ( euclidean == 0 ) { zeroMeters }
		else { math.sqrt(euclidean).voxels }
	}
	def euclidDistanceTo ( tc : TMajorCoord ) = {
		val c = tc.toVoxelCoord
		((this.x-c.x) * (this.x-c.x) + (this.y-c.y) * (this.y-c.y) + (this.z-c.z) * (this.z-c.z)).voxels
	}

	def toStringer = "VoxelCoord(" + x + "," + y + "," + z + ")"

	def fastEquals ( v : VoxelCoord ) = {
		v.x == this.x && v.y == this.y && v.z == this.z
	}
}



object VoxelCoord {
	/** Returns a VoxelCoord at Center, modified by the provided arguments */
	def fromRelative(dx: Int, dy: Int, dz: Int) = {
		VoxelCoord(Center.x + dx,Center.y + dy,Center.z + dz)
	}


	def apply ( v: ReadVec3i ) : VoxelCoord = new ConstVoxelCoord(v.x,v.y,v.z)
	def apply ( x : Int,y : Int,z : Int ) : VoxelCoord = new ConstVoxelCoord(x,y,z)
	def apply ( xy : ReadVec2i ,z : Int ) : VoxelCoord = new ConstVoxelCoord(xy.x,xy.y,z)
	def apply ( ap : (Int,Int), bp : (Int,Int), cp : (Int,Int)) : VoxelCoord = {
		val v = MutableVoxelCoord(0,0,0)
		v(ap._1) = ap._2
		v(bp._1) = bp._2
		v(cp._1) = cp._2
		v
	}
	def unapply ( v : VoxelCoord ) : Option[(Int,Int,Int)] = Some((v.x,v.y,v.z))
	implicit def toVec2i ( vcoord : VoxelCoord ) : Vec2i = { vcoord.xy }

	def centroid ( t : Traversable[VoxelCoord] ) : VoxelCoord = {
		val sum = MutableVoxelCoord(0,0,0)
		var counter = 0
		for ( v <- t ) {
			sum.x += v.x
			sum.y += v.y
			sum.z += v.z
			counter += 1
		}
		if ( counter > 0 ) {
			sum.x /= counter
			sum.y /= counter
			sum.z /= counter
		}
		sum
	}

	def centroid ( t : Traversable[VoxelCoord] , summer : MutableVoxelCoord ) : VoxelCoord = {
		val sum = summer
		var counter = 0
		for ( v <- t ) {
			sum.x += v.x
			sum.y += v.y
			sum.z += v.z
			counter += 1
		}
		if ( counter > 0 ) {
			sum.x /= counter
			sum.y /= counter
			sum.z /= counter
		}
		sum
	}

	@inline
	def hash ( x : Int, y : Int , z : Int ) = {
		41 * (
	      41 * (
	        41 + z
	      ) + y
	    ) + x
	}

	@inline
	def hashL ( x : Long, y : Long, z : Long) : Long = {
		(z << 40L) + (y << 20L) + x
	}

	import scalaxy.loops._
	def forAllAdjacentUnsafe(v : VoxelCoord,f : (VoxelCoord) => Boolean) : Boolean = {
		val mut = new MutableVoxelCoord()
		for (q <- 0 until 6 optimized) {
			mut.x = v.x + cardinalsX(q)
			mut.y = v.y + cardinalsY(q)
			mut.z = v.z + cardinalsZ(q)
			if (!f(v)) { return false }
		}
		true
	}
	def forAnyAdjacentUnsafe(v : VoxelCoord,f : (VoxelCoord) => Boolean) : Boolean = {
		val mut = new MutableVoxelCoord()
		for (q <- 0 until 6 optimized) {
			mut.x = v.x + cardinalsX(q)
			mut.y = v.y + cardinalsY(q)
			mut.z = v.z + cardinalsZ(q)
			if (f(v)) { return true }
		}
		false
	}

	val Center : VoxelCoord = new MutableVoxelCoord(2048,2048,2048)

	val Sentinel : VoxelCoord = new SentinelVoxelCoord
}