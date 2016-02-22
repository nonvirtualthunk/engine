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
	/* Must be vars because of bug https://issues.scala-lang.org/browse/SI-4511 */
	protected[datastructures] var dummyTalea = new Talea[T](VoxelCoord(-1, -1, -1), defaultValue)
	protected[datastructures] var _grid = new RawGrid[Talea[T]](VoxelCoord.Center, coreSize, createTalea)
	def grid = _grid

	override def apply(x: Int, y: Int, z: Int): T = {
		val talea = grid.getOrElse(x, y, z, dummyTalea)
		talea(x - talea.x, y - talea.y, z - talea.z)
	}

	override def update(x: Int, y: Int, z: Int, value: T): Unit = {
		val talea = _grid.getOrElseUpdate(x, y, z)
		talea(x - talea.x, y - talea.y, z - talea.z) = value
	}

	protected[datastructures] def createTalea(x: Int, y: Int, z: Int) = {
		new Talea[T](VoxelCoord(x, y, z), defaultValue)
	}

	override def subStore(region: VoxelRegion): BoundedVoxelStore[T] = {
		val min = region.min
		val max = region.max

		val shiftedMin = min >> Talea.dimensionPo2
		val shiftedMax = max >> Talea.dimensionPo2

		// if min and max are in the same talea, we can just return the talea itself
		if (shiftedMin == shiftedMax) {
			new SingleTaleaWrapper[T](grid.getOrElseUpdate(min.x, min.y, min.z))
		} else {
			val shiftedDim = shiftedMax - shiftedMin
			if (shiftedDim <= 1) {
				new TaleaBlock2x2[T](this, shiftedMin << Talea.dimensionPo2)
			} else {
				???
			}
		}
	}
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = {
		subStore(region)
	}
}

class SingleTaleaWrapper[@specialized(Byte, Short, Int) T](talea : Talea[T]) extends BoundedVoxelStore[T] {
	private[this] final val tx = talea.position.x
	private[this] final val ty = talea.position.y
	private[this] final val tz = talea.position.z
	override def update(x: Int, y: Int, z: Int, value: T): Unit = talea(x - tx, y - ty, z - tz) = value
	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = this
	val _region = VoxelRegion(talea.position, talea.position + (Talea.dimension-1))
	override def region: VoxelRegion = _region
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = this
	override def apply(x: Int, y: Int, z: Int): T = talea(x - tx,y - ty,z - tz)
	override def foreach(f: (Int, Int, Int, T) => Unit): Unit = {
		talea.foreach((x,y,z,b) => {
			f(x+tx,y+ty,z+tz,b)
		})
	}
}

class TaleaBlock2x2[@specialized(Byte, Short, Int) T](vgrid: VoxelGrid[T], alignedMin: VoxelCoord) extends BoundedVoxelStore[T] {
	val taleae = Array.ofDim[AnyRef](8)
	for (x <- 0 to 1 optimized; y <- 0 to 1 optimized; z <- 0 to 1 optimized) {
		taleae((x << 2) + (y << 1) + z) = vgrid.grid.getOrElse(
			alignedMin.x + (x << Talea.dimensionPo2),
			alignedMin.y + (y << Talea.dimensionPo2),
			alignedMin.z + (z << Talea.dimensionPo2),
			vgrid.dummyTalea)
	}
	var _region = VoxelRegion(alignedMin, alignedMin + (Talea.dimension * 2 - 1))
	def region = _region

	override def update(x: Int, y: Int, z: Int, value: T): Unit = {
		val rx = x - alignedMin.x
		val ry = y - alignedMin.y
		val rz = z - alignedMin.z
		val sx = rx >> Talea.dimensionPo2
		val sy = ry >> Talea.dimensionPo2
		val sz = rz >> Talea.dimensionPo2

		val idx = (sx << 2) + (sy << 1) + sz
		val talea = taleae(idx).asInstanceOf[Talea[T]]
		if (talea eq vgrid.dummyTalea) {
			val newTalea = vgrid.grid.getOrElseUpdate(
				alignedMin.x + (sx << Talea.dimensionPo2),
				alignedMin.y + (sy << Talea.dimensionPo2),
				alignedMin.z + (sz << Talea.dimensionPo2))
			taleae(idx) = newTalea
			newTalea(x - newTalea.x, y - newTalea.y, z - newTalea.z) = value
		} else {
			talea(x - talea.x, y - talea.y, z - talea.z) = value
		}
	}
	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = this
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = this
	override def apply(x: Int, y: Int, z: Int): T = {
		val rx = x - alignedMin.x
		val ry = y - alignedMin.y
		val rz = z - alignedMin.z
		val sx = rx >> Talea.dimensionPo2
		val sy = ry >> Talea.dimensionPo2
		val sz = rz >> Talea.dimensionPo2

		val talea = taleae((sx << 2) + (sy << 1) + sz).asInstanceOf[Talea[T]]
		talea(x - talea.x, y - talea.y, z - talea.z)
	}
}