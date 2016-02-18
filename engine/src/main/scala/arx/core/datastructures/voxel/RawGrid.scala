package arx.core.datastructures.voxel

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/12/16
  * Time: 8:05 AM
  */

import arx.Prelude._
import arx.core.datastructures.AtomicMapi
import arx.core.serialization.ArxKryoSerializer
import arx.core.vec.ReadVec3i
import arx.core.vec.ReadVec3i
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.VoxelCoord
import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.io.Output
import org.cliffc.high_scale_lib.ArxUnsafeUtil
import scalaxy.loops._
import arx.core.vec._

class RawGrid[TaleaType <: AnyRef](val origin: VoxelCoord, val coreSize: ReadVec3i, val creator : (Int,Int,Int) => TaleaType) {
	private[this] final val ox = origin.x
	private[this] final val oy = origin.y
	private[this] final val oz = origin.z
	private[this] final val coreLogSize = computeCoreLogSize()
	private[this] final val coreTaleaSize = coreSize / Talea.dimension
	private[this] final val rawArray: Array[AnyRef] = Array.ofDim[AnyRef](coreTaleaSize.x * coreTaleaSize.y * coreTaleaSize.z)
	private[this] final val extraTaleae = new AtomicMapi[Int, TaleaType]()

	private[this] final val xshift = coreLogSize.y + coreLogSize.z
	private[this] final val yshift = coreLogSize.z

	@transient private[this] val unsafe = ArxUnsafeUtil.getUnsafe
	@transient private[this] val arrayOffset = unsafe.arrayBaseOffset(classOf[Array[Object]])
	@transient private[this] val arrayScale = unsafe.arrayIndexScale(classOf[Array[Object]])
	private[this] val dimensionPo2 = Talea.dimensionPo2

	def updateRawArray(idx: Int, tmp: TaleaType) : TaleaType = {
		//Array | Index | Old | New
		if (unsafe.compareAndSwapObject(rawArray, arrayOffset + idx * arrayScale, null, tmp)) {
			tmp.asInstanceOf[TaleaType]
		} else {
			rawArray(idx).asInstanceOf[TaleaType]
		}
	}


	def getOrElseUpdate(x: Int, y: Int, z: Int) : TaleaType = {
		val dpo2 = dimensionPo2
		val nx = (x - ox) >> dpo2
		val ny = (y - oy) >> dpo2
		val nz = (z - oz) >> dpo2

		// we could theoretically consider catching the exception rather than double checking, probably wouldn't be
		// faster though
		if (nx >= 0 && ny >= 0 && nz >= 0 && nx < coreTaleaSize.x && ny < coreTaleaSize.y && nz < coreTaleaSize.z) {
			val idx = (nx << xshift) + (ny << yshift) + (nz)
			val cur = rawArray(idx)
			if (cur != null) {
				cur.asInstanceOf[TaleaType]
			} else {
				val tmp: TaleaType = creator(nx << dpo2,ny << dpo2, nz << dpo2)
				updateRawArray(idx, tmp)
			}
		} else {
			extraTaleae.getOrElseUpdate(Talea.hash(x, y, z), creator(x,y,z))
		}
	}
	def getOrElse(x: Int, y: Int, z: Int, u: TaleaType) : TaleaType = {
		val nx = (x - origin.x) >> Talea.dimensionPo2
		val ny = (y - origin.y) >> Talea.dimensionPo2
		val nz = (z - origin.z) >> Talea.dimensionPo2

		if (nx >= 0 && ny >= 0 && nz >= 0 && nx < coreTaleaSize.x && ny < coreTaleaSize.y && nz < coreTaleaSize.z) {
			val idx = (nx << xshift) + (ny << yshift) + (nz)
			val cur = rawArray(idx)
			if (cur != null) {
				cur.asInstanceOf[TaleaType]
			} else {
				u.asInstanceOf[TaleaType]
			}
		} else {
			extraTaleae.getOrElse(Talea.hash(x, y, z), u)
		}
	}

	def contains(x: Int, y: Int, z: Int) = {
		val nx = (x - origin.x) >> Talea.dimensionPo2
		val ny = (y - origin.y) >> Talea.dimensionPo2
		val nz = (z - origin.z) >> Talea.dimensionPo2

		val idx = (nx << xshift) + (ny << yshift) + (nz)
		rawArray(idx) != null
	}

	def put(x: Int, y: Int, z: Int, t: TaleaType) = {
		val nx = (x - origin.x) >> Talea.dimensionPo2
		val ny = (y - origin.y) >> Talea.dimensionPo2
		val nz = (z - origin.z) >> Talea.dimensionPo2

		if (nx >= 0 && ny >= 0 && nz >= 0 && nx < coreTaleaSize.x && ny < coreTaleaSize.y && nz < coreTaleaSize.z) {
			val idx = (nx << xshift) + (ny << yshift) + (nz)
			rawArray(idx) = t
		} else {
			extraTaleae.put(Talea.hash(x, y, z), t)
		}
	}

	def get(x: Int, y: Int, z: Int) = {
		val nx = (x - origin.x) >> Talea.dimensionPo2
		val ny = (y - origin.y) >> Talea.dimensionPo2
		val nz = (z - origin.z) >> Talea.dimensionPo2

		if (nx >= 0 && ny >= 0 && nz >= 0 && nx < coreTaleaSize.x && ny < coreTaleaSize.y && nz < coreTaleaSize.z) {
			val idx = (nx << xshift) + (ny << yshift) + (nz)
			val cur = rawArray(idx)
			if (cur == null) {
				None
			} else {
				Some(cur)
			}
		} else {
			extraTaleae.get(Talea.hash(x, y, z))
		}
	}

	def values = rawArray.filterNot(_ == null).toList ::: extraTaleae.values.toList

	def getRawArray = rawArray

	def computeCoreLogSize() = {
		Vec3i((math.log(coreSize.x / Talea.dimension) / math.log(2)).intValue,
			(math.log(coreSize.y / Talea.dimension) / math.log(2)).intValue,
			(math.log(coreSize.z / Talea.dimension) / math.log(2)).intValue)
	}

	//	def writeExternal(p1: ObjectOutput) {
	//		val stream = new ArxOutputStream(p1)
	//		stream.write(origin)
	//		stream.write(coreSize)
	//		stream.write(coreLogSize)
	//		stream.write(coreTaleaSize)
	//		stream.writeNBHMi(extraTaleae)
	//		stream.write(xshift)
	//		stream.write(yshift)
	//
	//		stream.write(rawArray)
	//	}
	//
	//	def readExternal(p1: ObjectInput) {
	//		val stream = new ArxInputStream(p1)
	//		origin = stream.read
	//		ox = origin.x;
	//		oy = origin.y;
	//		oz = origin.z
	//		coreSize = stream.read
	//		coreLogSize = stream.read
	//		coreTaleaSize = stream.read
	//		extraTaleae = stream.readNBHMi
	//		xshift = stream.readInt
	//		yshift = stream.readInt
	//
	//		rawArray = stream.read
	//	}
}

class RawGridSerializer extends ArxKryoSerializer[RawGrid[_]] {
	override def write(kryo: Kryo, out: Output, t: RawGrid[_]): Unit = {
		kryo.writeClassAndObject(out, t.origin)
		kryo.writeClassAndObject(out, t.coreSize)
		kryo.writeClassAndObject(out, t.getRawArray)
	}
	override def read(kryo: Kryo, in: Input, aClass: Class[RawGrid[_]]): RawGrid[_] = {
		val origin = kryo.readClassAndObject(in).asInstanceOf[VoxelCoord]
		val coreSize = kryo.readClassAndObject(in).asInstanceOf[ReadVec3i]
		val inArr = kryo.readClassAndObject(in).asInstanceOf[Array[AnyRef]]

		val ret = new RawGrid[AnyRef](origin, coreSize)
		val retArr = ret.getRawArray

		Array.copy(inArr,0,retArr,0,retArr.length)
		return ret;
	}
}