package arx.game.data

/**
 * TODO: Add javadoc
 */

import arx.core.vec.ReadVec4f
import arx.core.vec.coordinates.{ObjectCoord, VoxelCoord}
import arx.engine.data.TWorldAuxData
import arx.engine.world.World

class DebugData extends TWorldAuxData {
	var active = false
	var voxelGroups = Map[String,DebugData.VoxelGroup]()
	var pointGroups = Map[String,DebugData.PointGroup]()
	var graphingData = List[(Float,Float)]()
}


object DebugData {
	var worldRef = World.Sentinel

	case class VoxelGroup (color : ReadVec4f, voxels : Set[VoxelCoord])
	case class PointGroup (color : ReadVec4f, connected : Boolean, points : List[ObjectCoord])
}
