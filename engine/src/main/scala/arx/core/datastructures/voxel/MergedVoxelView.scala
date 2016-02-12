package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/8/16
  * Time: 7:13 AM
  */

import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion

import scalaxy.loops._

class MergedInfiniteVoxelView[T](views: Array[VoxelView[T]], nullValue: T) extends VoxelView[T] {
	override def apply(x: Int, y: Int, z: Int): T = {
		for (i <- 0 until views.length optimized) {
			val tmp = views(i)(x, y, z)
			if (tmp != nullValue) {
				return tmp
			}
		}
		nullValue
	}

	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] =
		new MergedInfiniteVoxelView[T](views, nullValue) with BoundedVoxelView[T] {
			override def region: VoxelRegion = region
		}
}

class DifferenceInfiniteVoxelView[T](baseView: VoxelView[T], differenceView: VoxelView[T], nullValue: T) extends VoxelView[T] {
	override def apply(x: Int, y: Int, z: Int): T = {
		if (differenceView(x, y, z) != nullValue) {
			nullValue
		} else {
			baseView(x, y, z)
		}
	}

	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] =
		new DifferenceInfiniteVoxelView[T](baseView, differenceView, nullValue) with BoundedVoxelView[T] {
			override def region: VoxelRegion = region
		}
}

class FunctionVoxelView[T](func: (Int, Int, Int) => T) extends VoxelView[T] {
	override def apply(x: Int, y: Int, z: Int): T = func(x, y, z)

	override def subView(subRegion: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] =
		new FunctionVoxelView[T](func) with BoundedVoxelView[T] {
			val region = subRegion
		}
}
