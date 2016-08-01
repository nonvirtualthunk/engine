package arx.core.datastructures.voxel

import arx.core.datastructures.voxelregions.voxelregions.EmptyVoxelRegion
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/17/16
  * Time: 8:18 AM
  */

trait VoxelView[@specialized(Byte, Short, Int) T] {
	def apply(x: Int, y: Int, z: Int): T
	def apply(v: VoxelCoord): T = apply(v.x, v.y, v.z)

	def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T]
}

trait VoxelStore[@specialized(Byte, Short, Int) T] extends VoxelView[T] {
	def update(x: Int, y: Int, z: Int, value: T)
	def update(v: VoxelCoord, value: T): Unit = {
		update(v.x, v.y, v.z, value)
	}

	def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T]

	// optional functions
	def setIfEqual(x : Int, y : Int, z : Int, oldV : T, newV : T) = {
		val cur = this.apply(x,y,z)
		if (cur == oldV) {
			this.update(x,y,z,newV)
		}
		cur
	}
	def setIfNotEqual ( x : Int, y : Int,z : Int, oldV : T, newV : T ) : T = {
		val cur = this.apply(x,y,z)
		if (cur != oldV) {
			this.update(x,y,z,newV)
		}
		cur
	}
}

trait BoundedVoxelView[@specialized(Byte, Short, Int) T] extends VoxelView[T] {
	def region: VoxelRegion

	def foreach(f: (Int, Int, Int, T) => Unit): Unit = {
		region.foreachUnsafe(v => {
			f(v.x, v.y, v.z, this.apply(v))
		})
	}
}

trait BoundedVoxelStore[@specialized(Byte, Short, Int) T] extends VoxelStore[T] with BoundedVoxelView[T] {

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