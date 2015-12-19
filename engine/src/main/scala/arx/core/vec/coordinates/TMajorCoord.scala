package arx.core.vec.coordinates

import arx.core.traits.TSentinelable
import arx.core.units.UnitOfDistance

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/15
 * Time: 10:32 AM
 */

trait TMajorCoord extends TSentinelable {
	 def toObjectCoord : ObjectCoord
	 def toObjectCoordFoot : ObjectCoord
	 def toVoxelCoord : VoxelCoord
	 def distanceTo ( tc : TMajorCoord ) : UnitOfDistance
	 def euclidDistanceTo ( tc : TMajorCoord ) : UnitOfDistance
 }
