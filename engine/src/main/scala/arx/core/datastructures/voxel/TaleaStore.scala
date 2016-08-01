package arx.core.datastructures.voxel

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.vec.coordinates.VoxelCoord
import scala.collection.TraversableLike
import scalaxy.loops._

trait TaleaStore[@specialized(Byte,Short,Int) T] {
	def taleaAtRO(x:Int, y:Int, z:Int) : TTalea[T]
	def taleaAtRO(v : VoxelCoord) : TTalea[T] = {
		taleaAtRO(v.x,v.y,v.z)
	}

	def taleaAt(x:Int, y:Int, z:Int) : TTalea[T]
	def taleaAt(v : VoxelCoord) : TTalea[T] = {
		taleaAt(v.x,v.y,v.z)
	}

	def allTaleae : Traversable[TTalea[T]]
}
