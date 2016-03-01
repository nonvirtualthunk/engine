package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/8/16
  * Time: 6:57 AM
  */

import arx.Prelude._
import arx.core.vec.ReadVec3i
import arx.core.vec.coordinates.VoxelCoord
import scalaxy.loops._

trait TTalea[@specialized(Byte,Short,Int) T] extends VoxelStore[T] {
	var _modifiedCount: Int = 0
	var _edgeModifiedCount : Array[Int] = Array.ofDim[Int](6) //lx hx , ly hy , lz hy
	var isLoggingEnabled = false

	def defaultValue : T

	def position : VoxelCoord
	def size : Int

	def x: Int = position.x
	def y: Int = position.y
	def z: Int = position.z

	/** Gets the value at the given coordinates and decrements it, to a minimum of 0 */
	def getAndDecrementToMinimumOf (x:Int,y:Int,z:Int,minimumValue : T):T
	def getBlock2x2 (x:Int,y:Int,z:Int,ret:Array[T]);
	def getWithAdj (x:Int,y:Int,z:Int,ret:Array[T])
	def setIfEqual ( x : Int, y : Int,z : Int, curV : T , newV : T ) : T
	def setIfNotEqual ( x : Int, y : Int,z : Int, curV : T, newV : T ) : T
	def setIfEqualRaw ( x : Int ,y  : Int,z : Int, curV : T , newV : T ) : T
	/** Sets voxel to v, if v > current, returns current */
	def setIfGreater ( x : Int , y : Int, z : Int, newV : T ) : T

	def setUnsafe ( x : Int , y : Int , z : Int , newV : T ) { update(x,y,z,newV) }
	def setUnsafeUnlogged ( x : Int , y : Int , z : Int , newV : T ) { update(x,y,z,newV) }

	def setAll (b: T)
	def areAll (b: T): Boolean
	def areNone (b : T): Boolean

	def memoryUsage: Int = { 0 }
	def loggedModifications : List[LoggedVoxelModification[T]]

	def incrementModifiedCount () { _modifiedCount += 1 }
	def modifiedCount: Int = _modifiedCount

	def edgeModifiedCount(i: Int): Int = _edgeModifiedCount(i)
	def edgeModifiedCount : Array[Int] =  _edgeModifiedCount

	def containsPoint ( v : ReadVec3i ) : Boolean = {
		v.x >= position.x && v.y >= position.y && v.z >= position.z &&
			v.x < position.x + size && v.y < position.y + size && v.z < position.z + size
	}

	def enableLogging () { isLoggingEnabled = true }
	def disableLogging () { isLoggingEnabled = false }
}
