package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/8/16
  * Time: 7:20 AM
  */

import java.io.ObjectInput
import java.io.ObjectOutput

import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import com.carrotsearch.hppc.LongByteOpenHashMap
import com.carrotsearch.hppc.LongObjectOpenHashMap

class HashVoxelStore[T](var defaultValue: T) extends VoxelStore[T] {
	def this() { this(null.asInstanceOf[T]) }
	var map = new LongObjectOpenHashMap[T]

	def hash(x: Int, y: Int, z: Int) = (x << 40) + (y << 20) + z
	def apply(x: Int, y: Int, z: Int) = {
		val base = map.get(hash(x, y, z))
		if (base == null) {defaultValue}
		else {base}
	}
	def update(x: Int, y: Int, z: Int, t: T) { map.put(hash(x, y, z), t) }

	def writeExternal(p1: ObjectOutput) { p1.writeObject(defaultValue); p1.writeObject(map); }
	def readExternal(p1: ObjectInput) { defaultValue = p1.readObject.asInstanceOf[T]; map = p1.readObject.asInstanceOf[LongObjectOpenHashMap[T]] }

	protected def sub(subRegion: VoxelRegion) = new HashVoxelStore[T](defaultValue) with BoundedVoxelView[T] {
		val region = subRegion
	}

	override def subStore(region: VoxelRegion): VoxelStore[T] with BoundedVoxelView[T] = sub(region)
	override def subView(region: VoxelRegion): VoxelView[T] with BoundedVoxelView[T] = sub(region)
}

class HashVoxelByteStore(var defaultValue: Byte) extends VoxelStore[Byte] {
	def this() { this(0.toByte) }
	var map = new LongByteOpenHashMap()

	def hash(x: Int, y: Int, z: Int) = (x << 40) + (y << 20) + z
	def apply(x: Int, y: Int, z: Int) = {
		if (map.containsKey(hash(x, y, z))) {
			map.lget()
		} else {
			defaultValue
		}
	}
	def update(x: Int, y: Int, z: Int, t: Byte) { map.put(hash(x, y, z), t) }

	def writeExternal(p1: ObjectOutput) { p1.writeByte(defaultValue); p1.writeObject(map); }
	def readExternal(p1: ObjectInput) { defaultValue = p1.readByte; map = p1.readObject.asInstanceOf[LongByteOpenHashMap] }

	protected def sub(subRegion: VoxelRegion) = new HashVoxelByteStore(defaultValue) with BoundedVoxelView[Byte] {
		val region = subRegion
	}

	override def subStore(region: VoxelRegion): VoxelStore[Byte] with BoundedVoxelView[Byte] = sub(region)
	override def subView(region: VoxelRegion): VoxelView[Byte] with BoundedVoxelView[Byte] = sub(region)
}