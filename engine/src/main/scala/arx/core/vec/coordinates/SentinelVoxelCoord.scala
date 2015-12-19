package arx.core.vec.coordinates

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/21/12
 * Time: 11:44 AM
 * Created by nonvirtualthunk
 */

import java.io.{ObjectInput, ObjectOutput}

import arx.core.traits.TSentinel

protected[coordinates] class SentinelVoxelCoord extends ConstVoxelCoord(-1,-1,-1) with TSentinel {
	override def writeExternal(out: ObjectOutput) {}
	override def readExternal(in: ObjectInput) {}
}