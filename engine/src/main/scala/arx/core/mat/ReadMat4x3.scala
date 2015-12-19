package arx.core.mat;
import arx.core.vec._
import arx.core.Moddable
import arx.core.SelfModdable
import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

@SerialVersionUID(9223372036854770000L)
class ReadMat4x3 extends Externalizable with SelfModdable[ReadMat4x3]{
	def this(m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m20i = m20a
		m30i = m30a
		m01i = m01a
		m11i = m11a
		m21i = m21a
		m31i = m31a
		m02i = m02a
		m12i = m12a
		m22i = m22a
		m32i = m32a
	}
protected var m00i : Float = 0.0f
protected var m10i : Float = 0.0f
protected var m20i : Float = 0.0f
protected var m30i : Float = 0.0f
protected var m01i : Float = 0.0f
protected var m11i : Float = 0.0f
protected var m21i : Float = 0.0f
protected var m31i : Float = 0.0f
protected var m02i : Float = 0.0f
protected var m12i : Float = 0.0f
protected var m22i : Float = 0.0f
protected var m32i : Float = 0.0f
	def +(m : Moddable[ReadMat4x3]) = { val v = m.resolve(); new ReadMat4x3(m00i + v.m00i,m10i + v.m10i,m20i + v.m20i,m30i + v.m30i,m01i + v.m01i,m11i + v.m11i,m21i + v.m21i,m31i + v.m31i,m02i + v.m02i,m12i + v.m12i,m22i + v.m22i,m32i + v.m32i) }
	def +(v : ReadMat4x3) = new ReadMat4x3(m00i + v.m00i,m10i + v.m10i,m20i + v.m20i,m30i + v.m30i,m01i + v.m01i,m11i + v.m11i,m21i + v.m21i,m31i + v.m31i,m02i + v.m02i,m12i + v.m12i,m22i + v.m22i,m32i + v.m32i)
	def -(m : Moddable[ReadMat4x3]) = { val v = m.resolve(); new ReadMat4x3(m00i - v.m00i,m10i - v.m10i,m20i - v.m20i,m30i - v.m30i,m01i - v.m01i,m11i - v.m11i,m21i - v.m21i,m31i - v.m31i,m02i - v.m02i,m12i - v.m12i,m22i - v.m22i,m32i - v.m32i) }
	def -(v : ReadMat4x3) = new ReadMat4x3(m00i - v.m00i,m10i - v.m10i,m20i - v.m20i,m30i - v.m30i,m01i - v.m01i,m11i - v.m11i,m21i - v.m21i,m31i - v.m31i,m02i - v.m02i,m12i - v.m12i,m22i - v.m22i,m32i - v.m32i)
	def +(s : Float) = new ReadMat4x3(m00i + s,m10i + s,m20i + s,m30i + s,m01i + s,m11i + s,m21i + s,m31i + s,m02i + s,m12i + s,m22i + s,m32i + s)
	def -(s : Float) = new ReadMat4x3(m00i - s,m10i - s,m20i - s,m30i - s,m01i - s,m11i - s,m21i - s,m31i - s,m02i - s,m12i - s,m22i - s,m32i - s)
	def *(s : Float) = new ReadMat4x3(m00i * s,m10i * s,m20i * s,m30i * s,m01i * s,m11i * s,m21i * s,m31i * s,m02i * s,m12i * s,m22i * s,m32i * s)
	def /(s : Float) = new ReadMat4x3(m00i / s,m10i / s,m20i / s,m30i / s,m01i / s,m11i / s,m21i / s,m31i / s,m02i / s,m12i / s,m22i / s,m32i / s)
	def * (other : ReadVec3f) : ReadVec4f = new ReadVec4f(
m00 * other.x + m01 * other.y + m02 * other.z,
m10 * other.x + m11 * other.y + m12 * other.z,
m20 * other.x + m21 * other.y + m22 * other.z,
m30 * other.x + m31 * other.y + m32 * other.z
)
	def * (other : ReadMat3x2) : ReadMat4x2 = new ReadMat4x2(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20,
m20 * other.m00 + m21 * other.m10 + m22 * other.m20,
m30 * other.m00 + m31 * other.m10 + m32 * other.m20,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21,
m20 * other.m01 + m21 * other.m11 + m22 * other.m21,
m30 * other.m01 + m31 * other.m11 + m32 * other.m21
)
	def * (other : ReadMat3x3) : ReadMat4x3 = new ReadMat4x3(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20,
m20 * other.m00 + m21 * other.m10 + m22 * other.m20,
m30 * other.m00 + m31 * other.m10 + m32 * other.m20,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21,
m20 * other.m01 + m21 * other.m11 + m22 * other.m21,
m30 * other.m01 + m31 * other.m11 + m32 * other.m21,
m00 * other.m02 + m01 * other.m12 + m02 * other.m22,
m10 * other.m02 + m11 * other.m12 + m12 * other.m22,
m20 * other.m02 + m21 * other.m12 + m22 * other.m22,
m30 * other.m02 + m31 * other.m12 + m32 * other.m22
)
	def * (other : ReadMat3x4) : ReadMat4x4 = new ReadMat4x4(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20,
m20 * other.m00 + m21 * other.m10 + m22 * other.m20,
m30 * other.m00 + m31 * other.m10 + m32 * other.m20,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21,
m20 * other.m01 + m21 * other.m11 + m22 * other.m21,
m30 * other.m01 + m31 * other.m11 + m32 * other.m21,
m00 * other.m02 + m01 * other.m12 + m02 * other.m22,
m10 * other.m02 + m11 * other.m12 + m12 * other.m22,
m20 * other.m02 + m21 * other.m12 + m22 * other.m22,
m30 * other.m02 + m31 * other.m12 + m32 * other.m22,
m00 * other.m03 + m01 * other.m13 + m02 * other.m23,
m10 * other.m03 + m11 * other.m13 + m12 * other.m23,
m20 * other.m03 + m21 * other.m13 + m22 * other.m23,
m30 * other.m03 + m31 * other.m13 + m32 * other.m23
)

	def readExternal ( in : ObjectInput ) {
m00i = in.readFloat
m10i = in.readFloat
m20i = in.readFloat
m30i = in.readFloat
m01i = in.readFloat
m11i = in.readFloat
m21i = in.readFloat
m31i = in.readFloat
m02i = in.readFloat
m12i = in.readFloat
m22i = in.readFloat
m32i = in.readFloat
}

	def writeExternal ( out : ObjectOutput ) {
out.writeFloat(m00i)
out.writeFloat(m10i)
out.writeFloat(m20i)
out.writeFloat(m30i)
out.writeFloat(m01i)
out.writeFloat(m11i)
out.writeFloat(m21i)
out.writeFloat(m31i)
out.writeFloat(m02i)
out.writeFloat(m12i)
out.writeFloat(m22i)
out.writeFloat(m32i)
}
	def m00 = m00i
	protected def m00_= ( s : Float ) { m00i = s }
	def m10 = m10i
	protected def m10_= ( s : Float ) { m10i = s }
	def m20 = m20i
	protected def m20_= ( s : Float ) { m20i = s }
	def m30 = m30i
	protected def m30_= ( s : Float ) { m30i = s }
	def m01 = m01i
	protected def m01_= ( s : Float ) { m01i = s }
	def m11 = m11i
	protected def m11_= ( s : Float ) { m11i = s }
	def m21 = m21i
	protected def m21_= ( s : Float ) { m21i = s }
	def m31 = m31i
	protected def m31_= ( s : Float ) { m31i = s }
	def m02 = m02i
	protected def m02_= ( s : Float ) { m02i = s }
	def m12 = m12i
	protected def m12_= ( s : Float ) { m12i = s }
	def m22 = m22i
	protected def m22_= ( s : Float ) { m22i = s }
	def m32 = m32i
	protected def m32_= ( s : Float ) { m32i = s }
	override def toString = "(" + m00 + "," + m10 + "," + m20 + "," + m30 + "," + m01 + "," + m11 + "," + m21 + "," + m31 + "," + m02 + "," + m12 + "," + m22 + "," + m32+ ")"
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadMat4x3 => m00 == v.m00 && m10 == v.m10 && m20 == v.m20 && m30 == v.m30 && m01 == v.m01 && m11 == v.m11 && m21 == v.m21 && m31 == v.m31 && m02 == v.m02 && m12 == v.m12 && m22 == v.m22 && m32 == v.m32
		case mv : Moddable[ReadMat4x3] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 + m00.hashCode) + m10.hashCode) + m20.hashCode) + m30.hashCode) + m01.hashCode) + m11.hashCode) + m21.hashCode) + m31.hashCode) + m02.hashCode) + m12.hashCode) + m22.hashCode) + m32.hashCode
}
object ReadMat4x3{
	def apply (m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float) = new ReadMat4x3(m00a : Float,m10a : Float,m20a : Float,m30a : Float,m01a : Float,m11a : Float,m21a : Float,m31a : Float,m02a : Float,m12a : Float,m22a : Float,m32a : Float)
	def apply (v : ReadMat4x3) = new ReadMat4x3(v.m00,v.m10,v.m20,v.m30,v.m01,v.m11,v.m21,v.m31,v.m02,v.m12,v.m22,v.m32)
}
