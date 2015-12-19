package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/18/13
 * Time: 10:28 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.traits.TVoxelStore
import arx.core.vec.{Vec3i, ReadVec3i}
import arx.core.vec.coordinates.VoxelCoord

trait FiniteThreeGrid[T] extends TVoxelStore[T] with Serializable {
	def apply ( x : Int , y : Int , z : Int ) : T
	def apply ( v : VoxelCoord ) : T = apply(v.x,v.y,v.z)

	def update ( x : Int , y : Int , z : Int , t : T )
	def update ( v : VoxelCoord , t : T ) { update(v.x,v.y,v.z,t) }

	def minimumPoint : VoxelCoord
	def maximumPoint : VoxelCoord

	def _setFromFunction ( f : (Int,Int,Int) => T ) {
		val min = minimumPoint
		val max = maximumPoint
		var x = min.x; while ( x <= max.x ) {
			var y = min.y; while ( y <= max.y ) {
				var z = min.z; while ( z <= max.z ) {
					this(x,y,z) = f(x,y,z)
				z += 1}
			y += 1}
		x += 1}
	}
}

@SerialVersionUID(1L)
class SimpleFiniteThreeGrid[T] ( twoToTheNDims : ReadVec3i, dims : ReadVec3i, backingArray : Array[T], sentinelValue : T ) extends FiniteThreeGrid[T] {
	protected val majorShift = twoToTheNDims.x + twoToTheNDims.y
	protected val minorShift = twoToTheNDims.x

	protected val ox = VoxelCoord.Center.x - dims.x / 2 //origin is at voxel coord center minus half the dims
	protected val oy = VoxelCoord.Center.y - dims.y / 2
	protected val oz = VoxelCoord.Center.z - dims.z / 2

	protected val mx = ox + dims.x //max is at voxel coord center plus half the dims
	protected val my = oy + dims.y
	protected val mz = oz + dims.z

	val minimumPoint = VoxelCoord(ox,oy,oz)
	val maximumPoint = VoxelCoord(mx-1,my-1,mz-1)

	@inline
	private final def index ( x : Int , y : Int , z : Int ) = ((z-oz) << majorShift) + ((y-oy) << minorShift) + (x-ox)

	@inline
	private final def checkIndices ( x : Int , y : Int , z : Int ) = {
		posit(x >= ox && y >= oy && z >= oz && x < mx && y < my && z < mz,
			f"Out of bounds in finite three grid, ($x,$y,$z), dims are $dims")
	}

	def apply(x: Int, y: Int, z: Int): T = {
		if ( x >= ox && y >= oy && z >= oz && x < mx && y < my && z < mz ) {
			backingArray( index(x,y,z) )
		} else {
			sentinelValue
		}
	}

	def update(x: Int, y: Int, z: Int, t: T) {
		if ( x >= ox && y >= oy && z >= oz && x < mx && y < my && z < mz ) {
			backingArray( index(x,y,z) ) = t
		}
	}
}

object FiniteThreeGrid {
	def apply[T : Manifest] ( twoToTheNDims : ReadVec3i, sentinel : T ) : FiniteThreeGrid[T] = {
		val dims = Vec3i(1 << twoToTheNDims.x,1 << twoToTheNDims.y,1 << twoToTheNDims.z)
		new SimpleFiniteThreeGrid[T]( twoToTheNDims, dims, manifest[T].newArray(dims.x * dims.y * dims.z), sentinel )
	}
}