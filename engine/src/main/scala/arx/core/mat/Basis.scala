package arx.core.mat

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/14/12
 * Time: 4:26 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.coordinates.ObjectCoord
import arx.core.vec.{Vec3f, ReadVec3f}
import java.io.{ObjectOutput, ObjectInput}
import arx.core.vec.Cardinals._

@SerialVersionUID(1L)
class Basis (var xaxis : ReadVec3f , var yaxis : ReadVec3f, var zaxis : ReadVec3f) extends Serializable {
	def this() { this(Vec3f.UnitX,Vec3f.UnitY,Vec3f.UnitZ) }
	def isIdentity = false
	def transform ( v : ReadVec3f ) : ReadVec3f = {
		val out = new Vec3f
		(
			xaxis.x * v.x + yaxis.x * v.y + zaxis.x * v.z,
			xaxis.y * v.x + yaxis.y * v.y + zaxis.y * v.z,
			xaxis.z * v.x + yaxis.z * v.y + zaxis.z * v.z
		)
		out
	}

	def transform ( v : ObjectCoord ) : ObjectCoord = {
		val out = ObjectCoord (
			xaxis.x * v.x + yaxis.x * v.y + zaxis.x * v.z,
			xaxis.y * v.x + yaxis.y * v.y + zaxis.y * v.z,
			xaxis.z * v.x + yaxis.z * v.y + zaxis.z * v.z
		)
		out
	}

	def transformAndAdd ( v : ObjectCoord, origin : ObjectCoord ) : ObjectCoord = {
		val out = ObjectCoord (
			xaxis.x * v.x + yaxis.x * v.y + zaxis.x * v.z + origin.x,
			xaxis.y * v.x + yaxis.y * v.y + zaxis.y * v.z + origin.y,
			xaxis.z * v.x + yaxis.z * v.y + zaxis.z * v.z + origin.z
		)
		out
	}
	def transformAndAdd ( v : ReadVec3f, origin : ReadVec3f ) : ReadVec3f = {
		val out = ReadVec3f (
			xaxis.x * v.x + yaxis.x * v.y + zaxis.x * v.z + origin.x,
			xaxis.y * v.x + yaxis.y * v.y + zaxis.y * v.z + origin.y,
			xaxis.z * v.x + yaxis.z * v.y + zaxis.z * v.z + origin.z
		)
		out
	}

	def subInvTransform ( v : ObjectCoord , origin : ObjectCoord ) : ObjectCoord = {
		val relativeV = v - origin
//		Mat3f(simplex3d.math.floatx.Vec3f(xaxis.x,xaxis.y,xaxis.z),simplex3d.math.floatx.Vec3f(yaxis,zaxis)
		Noto.warn("haven't actually implemented subInvTransform yet...just so you know, just throw a change of basis in there")
		relativeV
	}

	def writeExternal(out: ObjectOutput) {
		out.writeFloat(xaxis.x)
		out.writeFloat(xaxis.y)
		out.writeFloat(xaxis.z)

		out.writeFloat(yaxis.x)
		out.writeFloat(yaxis.y)
		out.writeFloat(yaxis.z)

		out.writeFloat(zaxis.x)
		out.writeFloat(zaxis.y)
		out.writeFloat(zaxis.z)
	}
	def readExternal(in: ObjectInput) {
		xaxis = ReadVec3f(in.readFloat,in.readFloat,in.readFloat)
		yaxis = ReadVec3f(in.readFloat,in.readFloat,in.readFloat)
		zaxis = ReadVec3f(in.readFloat,in.readFloat,in.readFloat)
	}

	override def toString = {
		"[\n" + xaxis.toString + "\n" + yaxis.toString + "\n" + zaxis.toString + "\n]"
	}
}
object IdentityBasis extends Basis(Vec3f.UnitX,Vec3f.UnitY,Vec3f.UnitZ) {//with SelfModdable[Basis] {
	override def isIdentity = true
	override def transform(v: ReadVec3f) = v
	override def transform(v: ObjectCoord) = v
	override def transformAndAdd(v: ObjectCoord, origin: ObjectCoord) = v + origin
	override def subInvTransform(v: ObjectCoord, origin: ObjectCoord) = v - origin

	def resolve() = this
	def baseValue() = this
}

trait TFrameOfReference {
	def isIdentity = false

	def transformFromFrameOfReference(v : ReadVec3f) : ReadVec3f
	def transformFromFrameOfReference(obj : ObjectCoord) : ObjectCoord

	def transformToFrameOfReference(obj : ObjectCoord) : ObjectCoord
}
case class PointFrameOfReference( point : ObjectCoord ) extends TFrameOfReference {
	def transformFromFrameOfReference(v: ReadVec3f): ReadVec3f = v + point
	def transformFromFrameOfReference(obj: ObjectCoord): ObjectCoord = obj + point
	def transformToFrameOfReference(obj: ObjectCoord): ObjectCoord = obj - point
}
object IdentityFrameOfReference extends TFrameOfReference {//with SelfModdable[TFrameOfReference] {
	override def isIdentity = true
	def transformFromFrameOfReference(v : ReadVec3f) : ReadVec3f = v
	def transformFromFrameOfReference(obj : ObjectCoord) : ObjectCoord = obj
	def transformToFrameOfReference(obj : ObjectCoord) : ObjectCoord = obj

	def resolve() = this
	def baseValue() = this
}

object Basis {
	def apply(xaxis : ReadVec3f , yaxis : ReadVec3f, zaxis : ReadVec3f)= new Basis(xaxis,yaxis,zaxis)
	val StandardBasis = IdentityBasis
	val StandardBases = (for ( i <- 0 until 6 ) yield {
		if ( i == Right ) { IdentityBasis }
		else { new Basis(XAxes(i),YAxes(i),ZAxes(i)) } }).toArray

	val Identity : Basis = IdentityBasis
}