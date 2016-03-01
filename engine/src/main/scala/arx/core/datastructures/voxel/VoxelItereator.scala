package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/26/16
  * Time: 7:52 AM
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
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

class Overrider[@specialized(Byte, Short, Int) T : Manifest](grid: VoxelGrid[T], region: VoxelRegion) {
	protected final var prevYRow = Array.ofDim[T](34)
	protected final var curYRow = Array.ofDim[T](34)
	protected final var nextYRow = Array.ofDim[T](34)
	protected final val underRow = Array.ofDim[T](32)
	protected final val overRow = Array.ofDim[T](32)
	protected final val out = Array.ofDim[T](7)

	var tx = 0
	var ty = 0
	var tz = 0

	var dx = 0
	var dy = 0
	var dz = 0


	def foreachWithAdjacents(): Unit = {
		val shiftedStart = region.min >> Talea.dimensionPo2
		val shiftedEnd = region.max >> Talea.dimensionPo2

		val writeOut = out

		val tp2 = Talea.dimensionPo2
		val td = Talea.dimension
		for (sx <- shiftedStart.x to shiftedEnd.x optimized; 
			  sy <- shiftedStart.y to shiftedEnd.y optimized;
			  sz <- shiftedStart.z to shiftedEnd.z optimized) {
			tx = sx << tp2
			ty = sy << tp2
			tz = sz << tp2

			dz = 0
			while (dz < td) {
				loadRowLong(tx, ty - 1, tz + dz, grid, prevYRow)
				loadRowLong(tx, ty, tz + dz, grid, curYRow)
				dy = 0
				while (dy < td) {
					loadRow(tx, ty + dy, tz + dz - 1, grid, underRow)
					loadRow(tx, ty + dy, tz + dz + 1, grid, overRow)
					loadRowLong(tx, ty + dy + 1, tz + dz, grid, nextYRow)

					writeOut(6) = curYRow(0)
					writeOut(3) = curYRow(1 )
					dx = 0
					while (dx < td) {
//						writeOut(0) = writeOut(6)
//						writeOut(6) = writeOut(3)
//						writeOut(3) = curYRow(dx+2)
//						writeOut(1) = prevYRow(dx+1)
//						writeOut(2) = underRow(dx)
//						writeOut(5) = overRow(dx)
//						writeOut(4) = nextYRow(dx+1)

						//0 -> left
						//1 -> back
						//2 -> bottom
						//3 -> right
						//4 -> front
						//5 -> top

						visit()
						dx += 1
					}

					val tmp = prevYRow
					prevYRow = curYRow
					curYRow = nextYRow
					nextYRow = tmp

					dy += 1
				}
				dz += 1
			}
		}
		after()
	}
	@inline def left = { curYRow(dx) }
	@inline def center = { curYRow(dx+1) }
	@inline def right = { curYRow(dx+2) }
	@inline def up = { overRow(dx) }
	@inline def down = { underRow(dx) }
	@inline def front = { nextYRow(dx+1) }
	@inline def back = { prevYRow(dx+1) }
	@inline def adj(i : Int) = {
//		i match {
//			case 0 => curYRow(dx)
//			case 1 => prevYRow(dx+1)
//			case 2 => underRow(dx)
//			case 3 => curYRow(dx+2)
//			case 4 => nextYRow(dx+1)
//			case 5 => overRow(dx)
//			case 6 => curYRow(dx+1)
//		}
		if (i == 0){ curYRow(dx) }
		else if (i == 1){ prevYRow(dx+1) }
		else if (i == 2){ underRow(dx) }
		else if (i == 3){ curYRow(dx+2) }
		else if (i == 4){ nextYRow(dx+1) }
		else if (i == 5){ overRow(dx) }
		else if (i == 6){ curYRow(dx+1) }
		else { curYRow(0) }
	}

	@inline
	protected def visit(): Unit = {

	}

	protected def after(): Unit = {

	}

	private[this] final def loadRow(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[T], arr: Array[T]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 0, arr)
	}

	private[this] final def loadRowLong(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[T], arr: Array[T]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 1, arr)
		arr(0) = grid(sx-1,sy,sz)
		arr(33) = grid(sx+Talea.dimension,sy,sz)
	}

}


class VoxelItereator2(grid: VoxelGrid[Byte], region: VoxelRegion) {
	var prevYRow = Array.ofDim[Byte](34)
	var curYRow = Array.ofDim[Byte](34)
	var nextYRow = Array.ofDim[Byte](34)
	val underRow = Array.ofDim[Byte](32)
	val overRow = Array.ofDim[Byte](32)

	var shiftedStart = region.min >> Talea.dimensionPo2
	var shiftedEnd = region.max >> Talea.dimensionPo2

	var sx = shiftedStart.x
	var sy = shiftedStart.y
	var sz = shiftedStart.z

	var tx = sx << Talea.dimensionPo2
	var ty = sy << Talea.dimensionPo2
	var tz = sz << Talea.dimensionPo2

	var dx = -1
	var dy = 0
	var dz = 0

	loadRowLong(tx, ty - 1, tz + dz, grid, prevYRow)
	loadRowLong(tx, ty, tz + dz, grid, curYRow)
	loadRow(tx, ty + dy, tz + dz - 1, grid, underRow)
	loadRow(tx, ty + dy, tz + dz + 1, grid, overRow)
	loadRowLong(tx, ty + dy + 1, tz + dz, grid, nextYRow)

	def next () : Boolean = {
		dx += 1
		if (dx >= Talea.dimension) {
			dx = 0
			dy += 1
			if (dy >= Talea.dimension) {
				dy = 0
				dz += 1
				if (dz >= Talea.dimension) {
					dz = 0
					sx += 1
					if (sx > shiftedEnd.x) {
						sx = shiftedStart.x
						sy += 1
						if (sy > shiftedEnd.y) {
							sy = shiftedStart.y
							sz += 1
							if (sz > shiftedEnd.z) {
								return false
							}
							tz = sz << Talea.dimensionPo2
						}
						ty = sy << Talea.dimensionPo2
					}
					tx = sx << Talea.dimensionPo2
				}
				loadRowLong(tx, ty - 1, tz + dz, grid, prevYRow)
				loadRowLong(tx, ty, tz + dz, grid, curYRow)
			}
			loadRow(tx, ty + dy, tz + dz - 1, grid, underRow)
			loadRow(tx, ty + dy, tz + dz + 1, grid, overRow)
			loadRowLong(tx, ty + dy + 1, tz + dz, grid, nextYRow)
		}
		true
	}

//	def foreachWithAdjacents(f: (Int, Int, Int,Int,Int,Int) => Unit): Unit = {
//
//
//		for (sx <- shiftedStart.x to shiftedEnd.x optimized; sy <- shiftedStart.y to shiftedEnd.y optimized;
//			  sz <- shiftedStart.z to shiftedEnd.z optimized) {
//			val tx = sx << Talea.dimensionPo2
//			val ty = sy << Talea.dimensionPo2
//			val tz = sz << Talea.dimensionPo2
//
//			for (dz <- 0 until Talea.dimension optimized) {
//				loadRowLong(tx, ty - 1, tz + dz, grid, prevYRow)
//				loadRowLong(tx, ty, tz + dz, grid, curYRow)
//				for (dy <- 0 until Talea.dimension optimized) {
//					loadRow(tx, ty + dy, tz + dz - 1, grid, underRow)
//					loadRow(tx, ty + dy, tz + dz + 1, grid, overRow)
//					loadRowLong(tx, ty + dy + 1, tz + dz, grid, nextYRow)
//
//					for (dx <- 0 until Talea.dimension optimized) {
////						out(Cardinals.Left) = curYRow(dx)
////						out(Cardinals.Center) = curYRow(dx+1)
////						out(Cardinals.Right) = curYRow(dx+2)
////						out(Cardinals.Back) = prevYRow(dx+1)
////						out(Cardinals.Bottom) = underRow(dx)
////						out(Cardinals.Top) = overRow(dx)
////						out(Cardinals.Front) = nextYRow(dx+1)
//
//
//						f(tx,ty,tz,dx,dy,dz)
//					}
//
//					val tmp = prevYRow
//					prevYRow = curYRow
//					curYRow = nextYRow
//					nextYRow = tmp
//				}
//			}
//		}
//	}



	protected def loadRow(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[Byte], arr: Array[Byte]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 0, arr)
	}

	protected def loadRowLong(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[Byte], arr: Array[Byte]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 1, arr)
		arr(0) = grid(sx-1,sy,sz)
		arr(33) = grid(sx+Talea.dimension,sy,sz)
	}


}
