package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/8/16
  * Time: 7:03 AM
  */

import arx.Prelude._
import arx.core.vec.coordinates.VoxelCoord
import org.scalatest.FlatSpec
import scalaxy.loops._
import arx.core.vec._

class TaleaTests extends FlatSpec {

	"Talea iteration" should "access every voxel exactly once" in {
		val talea = new Talea[Int](VoxelCoord.Center, 0)

		for (x <- 0 until talea.size; y <- 0 until talea.size; z <- 0 until talea.size) {
			talea(x,y,z) = z * talea.size * talea.size + y * talea.size + x
		}

		var seen = Set[Int]()
		talea.foreach((x,y,z, i) => {
			seen += i
		})

		for (i <- 0 until talea.size * talea.size * talea.size) {
			assert(seen.contains(i))
		}
	}

}
