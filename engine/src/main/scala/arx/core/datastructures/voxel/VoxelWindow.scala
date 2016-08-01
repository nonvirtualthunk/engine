package arx.core.datastructures.voxel

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
import scalaxy.loops._

class VoxelWindow[@specialized(Byte,Short,Int) T](grid : VoxelGrid[T], relativeTo : VoxelCoord) extends VoxelStore[T] with TaleaStore[T] {
	private final val v = relativeTo

	override def update(x: Int, y: Int, z: Int, value: T): Unit = grid.update(v.x + x,v.y + y,v.z + z, value)

	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = ???

	override def taleaAt(x: Int, y: Int, z: Int): TTalea[T] = grid.taleaAt(v.x + x,v.y + y,v.z + z)

	override def taleaAtRO(x: Int, y: Int, z: Int): TTalea[T] = grid.taleaAtRO(v.x + x,v.y + y,v.z + z)

	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = ???

	override def apply(x: Int, y: Int, z: Int): T = grid.apply(v.x + x,v.y + y,v.z + z)

	override def allTaleae: Traversable[TTalea[T]] = ???
}

object VoxelWindow {
	def centeredOn[@specialized(Byte,Short,Int) T](grid : VoxelGrid[T], relativeTo : VoxelCoord) =
		new VoxelWindow[T](grid,relativeTo)
	def centeredOnTalea[@specialized(Byte,Short,Int) T](grid : VoxelGrid[T], relativeTo : VoxelCoord) =
		new VoxelWindow[T](grid,(relativeTo >> Talea.dimensionPo2) << Talea.dimensionPo2)
}
