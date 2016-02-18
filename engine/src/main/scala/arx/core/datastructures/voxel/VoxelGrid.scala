package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/7/16
  * Time: 3:42 PM
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.EmptyVoxelRegion
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.traits.TArxTraversable
import arx.core.vec.coordinates.VoxelCoord
import scala.language.postfixOps
import scalaxy.loops._
import arx.core.vec._

class VoxelGrid[@specialized(Byte, Short, Int) T](val defaultValue: T = null.asInstanceOf[T],
																  coreSize: ReadVec3i = Vec3i(2048)) extends VoxelStore[T] {
	protected[datastructures] val dummyTalea = new Talea[T](VoxelCoord(-1, -1, -1), defaultValue)
	protected[datastructures] val grid = new RawGrid[Talea[T]](VoxelCoord.Center, coreSize, createTalea)

	override def apply(x: Int, y: Int, z: Int): T = {
		val talea = grid.getOrElse(x, y, z, dummyTalea)
		talea(x - talea.x, y - talea.y, z - talea.z)
	}

	override def update(x: Int, y: Int, z: Int, value: T): Unit = {
		val talea = grid.getOrElseUpdate(x, y, z)
		talea(x - talea.x,y - talea.y,z - talea.z) = value
	}

	protected def createTalea(x: Int, y: Int, z: Int) = {
		new Talea[T](VoxelCoord(x,y,z), defaultValue)
	}

	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = {
		val min = region.min
		val max = region.max

		val shiftedMin = min >> Talea.dimensionPo2
		val shiftedMax = max >> Talea.dimensionPo2

		// if min and max are in the same talea, we can just return the talea itself
		if (shiftedMin == shiftedMax) {
			grid.getOrElse(min.x, min.y, min.z, dummyTalea)
		} else {
			val dim = max - min

		}
	}
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = {
		subStore(region)
	}
}


class TaleaBlock2x2[@specialized(Byte, Short, Int) T](vgrid : VoxelGrid, alignedMin : VoxelCoord) extends VoxelStore[T] with BoundedVoxelView[T] {
	val taleae = Array.ofDim[AnyRef](8)
	for (x <- 0 to 1 optimized; y <- 0 to 1 optimized; z <- 0 to 1 optimized) {
		taleae((x << 2) + (y << 1) + z) = vgrid.grid.getOrElse(
			alignedMin.x + (x << Talea.dimensionPo2),
			alignedMin.y + (y << Talea.dimensionPo2),
			alignedMin.z + (z << Talea.dimensionPo2),
			vgrid.dummyTalea)
	}

	override def update(x: Int, y: Int, z: Int, value: T): Unit = ???
	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = this
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = this
	override def apply(x: Int, y: Int, z: Int): T = {
		val rx = x - alignedMin.x
		val ry = y - alignedMin.y
		val rz = z - alignedMin.z
		val sx = rx >> Talea.dimensionPo2
		val sy = ry >> Talea.dimensionPo2
		val sz = rz >> Talea.dimensionPo2

		taleae
	}
}