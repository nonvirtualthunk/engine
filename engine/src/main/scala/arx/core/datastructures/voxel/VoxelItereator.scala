package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/26/16
  * Time: 7:52 AM
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import scala.language.postfixOps
import scalaxy.loops._
import arx.core.vec._

class VoxelItereator[@specialized(Byte, Short, Int) T : Manifest](grid: VoxelGrid[T], region: VoxelRegion) {

	def foreachWithAdjacents(f: (Int, Int, Int, Array[T]) => Unit): Unit = {
		val shiftedStart = region.min >> Talea.dimensionPo2
		val shiftedEnd = region.max >> Talea.dimensionPo2

		var prevYRow = Array.ofDim[T](34)
		var curYRow = Array.ofDim[T](34)
		var nextYRow = Array.ofDim[T](34)
		val underRow = Array.ofDim[T](32)
		val overRow = Array.ofDim[T](32)
		val out = Array.ofDim[T](7)

		for (sx <- shiftedStart.x to shiftedEnd.x optimized; sy <- shiftedStart.y to shiftedEnd.y optimized;
			  sz <- shiftedStart.z to shiftedEnd.z optimized) {
			val tx = sx << Talea.dimensionPo2
			val ty = sy << Talea.dimensionPo2
			val tz = sz << Talea.dimensionPo2

			for (dz <- 0 until Talea.dimension optimized) {
				loadRowLong(tx, ty - 1, tz + dz, grid, prevYRow)
				loadRowLong(tx, ty, tz + dz, grid, curYRow)
				for (dy <- 0 until Talea.dimension optimized) {
					loadRow(tx, ty + dy, tz + dz - 1, grid, underRow)
					loadRow(tx, ty + dy, tz + dz + 1, grid, overRow)
					loadRowLong(tx, ty + dy + 1, tz + dz, grid, nextYRow)

					for (dx <- 0 until Talea.dimension optimized) {
						out(Cardinals.Left) = curYRow(dx)
						out(Cardinals.Center) = curYRow(dx+1)
						out(Cardinals.Right) = curYRow(dx+2)
						out(Cardinals.Back) = prevYRow(dx+1)
						out(Cardinals.Bottom) = underRow(dx)
						out(Cardinals.Top) = overRow(dx)
						out(Cardinals.Front) = nextYRow(dx+1)


						f(tx+dx,ty+dy,tz+dz,out)
					}

					val tmp = prevYRow
					prevYRow = curYRow
					curYRow = nextYRow
					nextYRow = tmp
				}
			}
		}
	}



	protected def loadRow(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[T], arr: Array[T]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 0, arr)
	}

	protected def loadRowLong(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[T], arr: Array[T]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 1, arr)
		arr(0) = grid(sx-1,sy,sz)
		arr(33) = grid(sx+Talea.dimension,sy,sz)
	}


}
