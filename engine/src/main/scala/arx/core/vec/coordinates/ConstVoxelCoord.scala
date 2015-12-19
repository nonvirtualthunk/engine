package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/11/12
 * Time: 10:33 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.ReadVec3i

class ConstVoxelCoord(xa:Int,ya:Int,za:Int) extends ReadVec3i(xa,ya,za) with VoxelCoord {
	def this () { this(0,0,0) }
}