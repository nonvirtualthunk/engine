package arx.core.datastructures.voxel.iteration

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 12:20 PM
  */

import arx.Prelude._
import arx.core.datastructures.voxel.Talea
import arx.core.datastructures.voxel.VoxelGrid
import arx.core.introspection.ReflectionAssistant
import arx.core.vec.coordinates.VoxelCoord
import scalaxy.loops._
import arx.core.vec._

protected[datastructures] class SingleTaleaIterator[@specialized(Byte, Short, Int) T](grid: VoxelGrid[T], pos : VoxelCoord) {
	protected final var curRow = ReflectionAssistant.arrayOf(grid.dummyTalea.getComponentType, 32).asInstanceOf[Array[T]]

	var tx = pos.x
	var ty = pos.y
	var tz = pos.z
	val talea = grid.grid.getOrElse(tx,ty,tz,grid.dummyTalea)

	var dx = -1
	var dy = -1
	var dz = -1

	def moveTo(x : Int, y : Int, z : Int): Unit = {
		if (dz != z || dy != y) {
			loadRow(y, z, curRow)
		}
		dx = x
		dy = y
		dz = z
	}

	@inline def value = { curRow(dx) }

	private[this] final def loadRow(sy: Int, sz: Int, arr: Array[T]): Unit = {
		talea.loadRow(0,sy,sz,Talea.dimension, 0, arr)
	}

}