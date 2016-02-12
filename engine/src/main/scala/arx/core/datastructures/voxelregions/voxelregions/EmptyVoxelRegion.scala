package arx.core.datastructures.voxelregions.voxelregions

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/23/15
 * Time: 1:19 PM
 */

import arx.core.datastructures.voxel.EmptyByteVoxelGrid
import arx.core.datastructures.voxel.VoxelView
import arx.core.vec.coordinates.VoxelCoord

object EmptyVoxelRegion extends VoxelRegion {
	/** Region representing those voxels that are not contained within this region, but are adjacent to at least one voxel that is */
	override def adjacentVoxels: VoxelRegion = EmptyVoxelRegion
	override def performIntersection(other: VoxelRegion): VoxelRegion = EmptyVoxelRegion
	override def without (other : VoxelRegion) : VoxelRegion = EmptyVoxelRegion
	override def edgeVoxels: VoxelRegion = EmptyVoxelRegion
	override def boundingRegion: VoxelRegion = EmptyVoxelRegion
	override def max: VoxelCoord = VoxelCoord(0,0,0)
	override def asVoxelView: VoxelView[Byte] = EmptyByteVoxelGrid
	override def min: VoxelCoord = VoxelCoord(10000,10000,10000)
	override def contains(x: Int, y: Int, z: Int): Boolean = false
	override def foreachUnsafe[U](f: (VoxelCoord) => U): Unit = {}
	override def foreach[U](f: (VoxelCoord) => U): Unit = {}
	override def isEmpty = true

	override def hashCode(): Int = 1
	override def equals (other : Any) = other match {
		case a : AnyRef => a eq this
		case _ => false
	}
}
