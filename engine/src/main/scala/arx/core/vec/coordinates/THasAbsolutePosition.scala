package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/1/13
 * Time: 8:50 PM
 * Created by nonvirtualthunk
 */

import arx.core.traits.TSentinel

trait THasAbsolutePosition {
	def position : ObjectCoord
}

object THasAbsolutePosition{
	val Sentinel = new THasAbsolutePosition with TSentinel {
		def position: ObjectCoord = ObjectCoord.Sentinel
	}
}

trait THasVoxelPosition {
	def position : VoxelCoord
}