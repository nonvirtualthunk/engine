package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/8/16
  * Time: 7:03 AM
  */

import arx.Prelude._
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.metrics.Metrics
import arx.core.vec.coordinates.VoxelCoord
import org.scalatest.FlatSpec
import scala.language.postfixOps
import scalaxy.loops._
import arx.core.vec._

class TaleaTests extends FlatSpec {
	val c = VoxelCoord.Center

	"Talea iteration" should "access every voxel exactly once" in {
		val talea = new Talea[Int](VoxelCoord.Center, 0)

		for (x <- 0 until talea.size; y <- 0 until talea.size; z <- 0 until talea.size) {
			talea(x, y, z) = z * talea.size * talea.size + y * talea.size + x
		}

		var seen = Set[Int]()
		talea.foreach((x, y, z, i) => {
			seen += i
		})

		for (i <- 0 until talea.size * talea.size * talea.size) {
			assert(seen.contains(i))
		}
	}

	"Voxel grid" should "allow random access and update to its contents" in {
		val vgrid = new VoxelGrid[Int]()

		val defaultRead = vgrid(23, 12, 109)
		assert(defaultRead == 0)

		vgrid(23, 12, 109) = 17
		val updatedRead = vgrid(23, 12, 109)
		assert(updatedRead == 17)

		val v = VoxelCoord.Center + Vec3i(21, 45, 99)
		val inGridRead = vgrid(v)
		assert(inGridRead == 0)

		vgrid(v) = 22
		assert(vgrid(v) == 22)
	}

	"Voxel grid" should "be able to create small sub-views of itself" in {
		val vgrid = new VoxelGrid[Int](coreSize = Vec3i(256))

		vgrid(c + 130) = 55

		val r = VoxelRegion(c + 128, c + 145)
		val subView = vgrid.subView(r)
		assert(subView.region.contains(r.min) && subView.region.contains(r.max))

		assert(subView(c + 130) == 55)
	}

	"Voxel grid" should "be able to create medium sub-views of itself" in {
		val vgrid = new VoxelGrid[Short](coreSize = Vec3i(256))

		vgrid(c + 130) = 33

		val r = VoxelRegion(c + 128, c + 128 + (Talea.dimension * 1.5).toInt)
		val subView = vgrid.subStore(r)
		assert(subView.region.contains(r.min) && subView.region.contains(r.max))

		assert(subView(c + 130) == 33)
		subView(c + 128) = 9
		subView(c + 128 + Talea.dimension + 7) = 99
		assert(subView(c + 128) == 9)
		assert(subView(c + 128 + Talea.dimension + 7) == 99)
		assert(vgrid(c + 128 + Talea.dimension + 7) == 99)
	}

}

object PerformanceTestingVoxelGrid {
	def main(args: Array[String]) {

//		for (i <- 0 until 3 optimized) {
//			val grid = new VoxelGrid[Byte]
//
//			val c = VoxelCoord.Center
//			for (z <- 0 until 256 optimized; y <- 0 until 256 optimized; x <- 0 until 256 optimized) {
//				grid(c.x + x, c.y + y, c.z + z) = (x + y + z).toByte
//			}
//
//			var count = 0
//			Metrics.timer("Sequential top-level access, with adjacents").timeStmt {
//				for (z <- 1 until 255 optimized; y <- 1 until 255 optimized; x <- 1 until 255 optimized) {
//					val ax = c.x + x
//					val ay = c.y + y
//					val az = c.z + z
//					var sum: Int = grid(ax, ay, az)
//					var expected: Int = (x + y + z).toByte
//					for (q <- 0 until 6 optimized) {
//						sum += grid(ax + Cardinals.cardinalsX(q), ay + Cardinals.cardinalsY(q), az + Cardinals.cardinalsZ(q))
//						expected += (x + Cardinals.cardinalsX(q) + y + Cardinals.cardinalsY(q) + z + Cardinals.cardinalsZ(q)).toByte
//					}
//
//					if (sum != expected) {
//						println(s"Unexpected values $sum, $expected")
//					} else {
//						count += 1
//					}
//				}
//			}
//			println(s"count ${count}")
//		}
//		Metrics.prettyPrint()

		for (i <- 0 until 3 optimized) {
			adjOpt3()
		}

		for (i <- 0 until 3 optimized) {
			adjOpt4()
		}
		Metrics.prettyPrint()

	}

	def adjOpt1(): Unit = {
		val grid = new VoxelGrid[Byte]

		val c = VoxelCoord.Center
		for (z <- 0 until 256 optimized; y <- 0 until 256 optimized; x <- 0 until 256 optimized) {
			grid(c.x + x, c.y + y, c.z + z) = (x + y + z).toByte
		}

		val cardinalsX = Array(0, 0, 0, 0, 0)
		val cardinalsY = Array(-1, 0, 1, 0, 0)
		val cardinalsZ = Array(0, -1, 0, 1, 0)

		Metrics.timer("Sequential top-level access, with adjacents (opt)").timeStmt {
			for (z <- 1 until 255 optimized; y <- 1 until 255 optimized) {
				var curX = grid(c.x, c.y + y, c.z + z)
				var nextX = grid(c.x + 1, c.y + y, c.z + z)
				for (x <- 1 until 255 optimized) {
					val ax = c.x + x
					val ay = c.y + y
					val az = c.z + z
					var sum: Int = nextX + curX
					curX = nextX
					nextX = grid(ax + 1, ay, az)
					sum += nextX
					var expected = (ax + ay + az).toByte + (ax - 1 + ay + az).toByte + (ax + 1 + ay + az).toByte
					for (q <- 0 until 4 optimized) {
						sum += grid(ax + cardinalsX(q), ay + cardinalsY(q), az + cardinalsZ(q))
						expected += (x + cardinalsX(q) + y + cardinalsY(q) + z + cardinalsZ(q)).toByte
					}

					if (sum != expected) {
						println(s"Unexpected values $sum, $expected")
					}
				}
			}
		}
	}

	def adjOpt2(): Unit = {
		val grid = new VoxelGrid[Byte]

		val c = VoxelCoord.Center
		for (z <- -1 to 256 optimized; y <- -1 to 256 optimized; x <- -1 to 256 optimized) {
			grid(c.x + x, c.y + y, c.z + z) = (x + y + z).toByte
		}

		val cardinalsX = Array(0, 0, 0, 0, 0)
		val cardinalsY = Array(-1, 0, 1, 0, 0)
		val cardinalsZ = Array(0, -1, 0, 1, 0)

		val rows = Array.ofDim[Byte](4, 32)

		Metrics.timer("Sequential top-level access, with adjacents (opt)").timeStmt {
			for (z <- 1 until 255 optimized) {
				for (y <- 1 until 255 optimized) {
					for (bx <- 0 until 256 by 32 optimized) {
						val abx = c.x + bx

						for (q <- 0 until 4 optimized) {
							val az = c.z + z + cardinalsZ(q)
							val ay = c.y + y + cardinalsY(q)
							for (dx <- 0 until 32 optimized) {
								rows(q)(dx) = grid(abx + dx, ay, az)
							}
						}

						var curX = grid(abx - 1, c.y + y, c.z + z)
						var nextX = grid(abx, c.y + y, c.z + z)
						for (dx <- 0 until 32 optimized) {
							var sum: Int = curX + nextX
							curX = nextX
							nextX = grid(abx + dx + 1, c.y + y, c.z + z)
							sum += nextX

							//						var sum : Int = grid(abx+dx-1,c.y+y,c.z+z) + grid(abx+dx,c.y+y,c.z+z) + grid(abx+dx+1,c.y+y,c.z+z)
							val expected: Int = (abx + dx - 1 + c.y + y + c.z + z).toByte + (abx + dx + c.y + y + c.z + z).toByte + (abx + dx + 1 + c.y + y + c.z + z).toByte +
								(abx + dx + c.y - 1 + y + c.z + z).toByte + (abx + dx + c.y + y + 1 + c.z + z).toByte +
								(abx + dx + c.y + y + c.z + z - 1).toByte + (abx + dx + c.y + y + c.z + z + 1).toByte
							for (q <- 0 until 4 optimized) {
								sum += rows(q)(dx)
							}

							if (expected != sum) {
								println(s"Not matched ${expected}, $sum")
							}
						}
					}
				}
			}
		}
	}

	def adjOpt3(): Unit = {
		val grid = new VoxelGrid[Byte]

		val c = VoxelCoord.Center
		for (z <- -1 to 256 optimized; y <- -1 to 256 optimized; x <- -1 to 256 optimized) {
			grid(c.x + x, c.y + y, c.z + z) = (x + y + z).toByte
		}

		var prevYRow = Array.ofDim[Byte](34)
		var curYRow = Array.ofDim[Byte](34)
		var nextYRow = Array.ofDim[Byte](34)
		val underRow = Array.ofDim[Byte](32)
		val overRow = Array.ofDim[Byte](32)

		var count = 0
		Metrics.timer("Sequential top-level access, with adjacents (opt)").timeStmt {
			for (bz <- 0 until 256 by 32 optimized) {
				for (by <- 0 until 256 by 32 optimized) {
					for (bx <- 0 until 256 by 32 optimized) {
						// Talea starting points
						val tx = c.x + bx
						val ty = c.y + by
						val tz = c.z + bz

						for (dz <- 0 until Talea.dimension optimized) {
							loadRowLong(tx, ty - 1, tz + dz, grid, prevYRow)
							loadRowLong(tx, ty, tz + dz, grid, curYRow)
							for (dy <- 0 until Talea.dimension optimized) {
								loadRow(tx, ty + dy, tz + dz - 1, grid, underRow)
								loadRow(tx, ty + dy, tz + dz + 1, grid, overRow)
								loadRowLong(tx, ty + dy + 1, tz + dz, grid, nextYRow)

								for (dx <- 0 until Talea.dimension optimized) {
									val sum = curYRow(dx) + curYRow(dx + 1) + curYRow(dx + 2) + //because it's 34 long, offset by 1
										prevYRow(dx + 1) + nextYRow(dx + 1) +
										underRow(dx) + overRow(dx) // the under/over rows are not offset

									val expected = (tx + dx + ty + dy + tz + dz).toByte + (tx - 1 + dx + ty + dy + tz + dz).toByte + (tx + 1 + dx + ty + dy + tz + dz).toByte +
										(tx + dx + ty + dy - 1 + tz + dz).toByte + (tx + dx + ty + dy + 1 + tz + dz).toByte + (tx + dx + ty + dy + tz + dz - 1).toByte + (tx + dx + ty + dy + tz + dz + 1).toByte

									if (expected != sum) {
										println(s"Not matched $expected, $sum")
									} else {
										count += 1
									}
								}

								val tmp = prevYRow
								prevYRow = curYRow
								curYRow = nextYRow
								nextYRow = tmp
							}
						}
					}
				}
			}
		}
		println(s"count ${count}")
	}

	def adjOpt4(): Unit = {
		val grid = new VoxelGrid[Byte]

		val c = VoxelCoord.Center
		for (z <- -1 to 256 optimized; y <- -1 to 256 optimized; x <- -1 to 256 optimized) {
			grid(c.x + x, c.y + y, c.z + z) = (x + y + z).toByte
		}

		var count = 0
		val min = VoxelCoord(c)
		val max = VoxelCoord(c + 255)
		val iter = new VoxelItereator(grid, VoxelRegion(min,max))
		Metrics.timer("Opt 4").timeStmt {
			iter.foreachWithAdjacents((x,y,z,arr) => {
				val sum = arr(0) + arr(1) + arr(2) + arr(3) + arr(4) + arr(5) + arr(6)

				val expected = (x+y+z-1).toByte +(x+y+z+1).toByte + (x+y+z-1).toByte +(x+y+z+1).toByte +
					(x+y+z-1).toByte +(x+y+z+1).toByte + (x+y+z).toByte

				if (expected != sum) {
					println(s"Not matched $expected, $sum")
				} else {
					count += 1
				}
			})
		}
		println("Count: " + count)
	}

	def loadRow(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[Byte], arr: Array[Byte]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 0, arr)
	}

	def loadRowLong(sx: Int, sy: Int, sz: Int, grid: VoxelGrid[Byte], arr: Array[Byte]): Unit = {
		val talea = grid.grid.getOrElseUpdate(sx,sy,sz)
		val ty = sy - talea.y
		val tz = sz - talea.z
		talea.loadRow(0,ty,tz,Talea.dimension, 1, arr)
		arr(0) = grid(sx-1,sy,sz)
		arr(33) = grid(sx+Talea.dimension,sy,sz)
	}

}
