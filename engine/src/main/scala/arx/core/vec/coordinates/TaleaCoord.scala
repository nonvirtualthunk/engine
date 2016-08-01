package arx.core.vec.coordinates

/**
 * TODO: Add javadoc
 */

import arx.Prelude._
import arx.core.datastructures.voxel.Talea
import scalaxy.loops._

class TaleaCoord(v : VoxelCoord) extends ConstVoxelCoord(
	(v.x >> Talea.dimensionPo2) << Talea.dimensionPo2,
	(v.y >> Talea.dimensionPo2) << Talea.dimensionPo2,
	(v.z >> Talea.dimensionPo2) << Talea.dimensionPo2
)

object TaleaCoord {
	def apply(v : VoxelCoord) = new TaleaCoord(v)
}