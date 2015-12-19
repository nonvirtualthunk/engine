package arx.core.vec.coordinates

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/15
 * Time: 10:32 AM
 */
import arx.core.vec.Cardinals._

case class VoxelSideCoord( voxel : VoxelCoord , side : Int ) {
	 def adjacent = voxel + dirvec(side)
 }
