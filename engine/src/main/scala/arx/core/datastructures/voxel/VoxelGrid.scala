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
import scalaxy.loops._
import arx.core.vec._

class VoxelGrid[@specialized(Byte,Short,Int) T] extends VoxelStore[T] {
	override def apply(x: Int, y: Int, z: Int): T = ???

	override def update(x: Int, y: Int, z: Int, value: T): Unit = ???

	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = ???
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = {
		val dim = region.max - region.min
		if (dim < Talea.dimension) {
			val shiftedMin = region.min >> Talea.dimensionPo2
			val shiftedMax = region.max >> Talea.dimensionPo2


		}

	}
}

trait VoxelView[@specialized(Byte,Short,Int) T] {
	def apply(x : Int, y : Int, z : Int) : T
	def apply(v : VoxelCoord) : T = apply(v.x,v.y,v.z)

	def subView(region : VoxelRegion) : VoxelView[T] with BoundedVoxelView[T]
}

trait VoxelStore[@specialized(Byte, Short, Int) T] extends VoxelView[T] {
	def update(x : Int, y : Int, z : Int, value : T)
	def update(v : VoxelCoord, value : T): Unit = {
		update(v.x,v.y,v.z,value)
	}

	def subStore(region : VoxelRegion) : VoxelStore[T] with BoundedVoxelView[T]
}

trait BoundedVoxelView[@specialized(Byte, Short, Int) T] extends VoxelView[T] {
	def region : VoxelRegion

	def foreach(f : (Int,Int,Int,T) => Unit): Unit = {
		region.foreachUnsafe(v => {
			f(v.x,v.y,v.z, this.apply(v))
		})
	}
}

object EmptyByteVoxelGrid extends VoxelStore[Byte] with BoundedVoxelView[Byte] {
	override def update(x: Int, y: Int, z: Int, value: Byte): Unit = {}

	override def apply(x: Int, y: Int, z: Int): Byte = 0

	override def subStore(region: VoxelRegion): VoxelStore[Byte] with BoundedVoxelView[Byte] = {
		this
	}

	override def subView(region: VoxelRegion): VoxelView[Byte] with BoundedVoxelView[Byte] = {
		this
	}

	override def region: VoxelRegion = EmptyVoxelRegion
}