package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/28/16
  * Time: 12:12 PM
  */

import arx.Prelude._
import arx.core.datastructures.voxel.iteration.SingleTaleaIterator
import arx.core.datastructures.voxel.iteration.SingleTaleaWithAdjIterator
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
import scala.language.postfixOps
import scalaxy.loops._
import arx.core.vec._

object TaleaIterator {
	def withAdjacents[@specialized(Byte, Short, Int) T](grid : VoxelGrid[T], taleaPos : VoxelCoord) = {
		new SingleTaleaWithAdjIterator[T](grid, taleaPos)
	}

	def apply[@specialized(Byte, Short, Int) T](grid : VoxelGrid[T], taleaPos : VoxelCoord) = {
		new SingleTaleaIterator[T](grid, taleaPos)
	}

	// significantly slower...for some reason
//
//	def forAllIn[@specialized(Byte, Short, Int) T : Manifest](grid : VoxelGrid[T], region : VoxelRegion)(iterF : SingleTaleaIterator[T] => Unit): Unit = {
//		val shiftedStart = region.min >> Talea.dimensionPo2
//		val shiftedEnd = region.max >> Talea.dimensionPo2
//		for (sx <- shiftedStart.x to shiftedEnd.x optimized;
//			  sy <- shiftedStart.y to shiftedEnd.y optimized;
//			  sz <- shiftedStart.z to shiftedEnd.z optimized) {
//			val iter = TaleaIterator(grid,VoxelCoord(sx,sy,sz))
//			iterF(iter)
//		}
//	}
//
//	def forAllInWithAdjacents[@specialized(Byte, Short, Int) T : Manifest](grid : VoxelGrid[T], region : VoxelRegion)(iterF : SingleTaleaWithAdjIterator[T] => Unit): Unit = {
//		val shiftedStart = region.min >> Talea.dimensionPo2
//		val shiftedEnd = region.max >> Talea.dimensionPo2
//		for (sx <- shiftedStart.x to shiftedEnd.x optimized;
//			  sy <- shiftedStart.y to shiftedEnd.y optimized;
//			  sz <- shiftedStart.z to shiftedEnd.z optimized) {
//			val iter = TaleaIterator.withAdjacents(grid,VoxelCoord(sx,sy,sz))
//			iterF(iter)
//		}
//	}


}
