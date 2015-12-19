package arx.core.mat;
import arx.core.vec._
import arx.core.Moddable
import arx.core.SelfModdable
import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

@SerialVersionUID(9223372036854770000L)
class ReadMat3x4 extends Externalizable with SelfModdable[ReadMat3x4]{
	def this(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float,m03a : Float,m13a : Float,m23a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m20i = m20a
		m01i = m01a
		m11i = m11a
		m21i = m21a
		m02i = m02a
		m12i = m12a
		m22i = m22a
		m03i = m03a
		m13i = m13a
		m23i = m23a
	}
protected var m00i : Float = 0.0f
protected var m10i : Float = 0.0f
protected var m20i : Float = 0.0f
protected var m01i : Float = 0.0f
protected var m11i : Float = 0.0f
protected var m21i : Float = 0.0f
protected var m02i : Float = 0.0f
protected var m12i : Float = 0.0f
protected var m22i : Float = 0.0f
protected var m03i : Float = 0.0f
protected var m13i : Float = 0.0f
protected var m23i : Float = 0.0f
	def +(m : Moddable[ReadMat3x4]) = { val v = m.resolve(); new ReadMat3x4(m00i + v.m00i,m10i + v.m10i,m20i + v.m20i,m01i + v.m01i,m11i + v.m11i,m21i + v.m21i,m02i + v.m02i,m12i + v.m12i,m22i + v.m22i,m03i + v.m03i,m13i + v.m13i,m23i + v.m23i) }
	def +(v : ReadMat3x4) = new ReadMat3x4(m00i + v.m00i,m10i + v.m10i,m20i + v.m20i,m01i + v.m01i,m11i + v.m11i,m21i + v.m21i,m02i + v.m02i,m12i + v.m12i,m22i + v.m22i,m03i + v.m03i,m13i + v.m13i,m23i + v.m23i)
	def -(m : Moddable[ReadMat3x4]) = { val v = m.resolve(); new ReadMat3x4(m00i - v.m00i,m10i - v.m10i,m20i - v.m20i,m01i - v.m01i,m11i - v.m11i,m21i - v.m21i,m02i - v.m02i,m12i - v.m12i,m22i - v.m22i,m03i - v.m03i,m13i - v.m13i,m23i - v.m23i) }
	def -(v : ReadMat3x4) = new ReadMat3x4(m00i - v.m00i,m10i - v.m10i,m20i - v.m20i,m01i - v.m01i,m11i - v.m11i,m21i - v.m21i,m02i - v.m02i,m12i - v.m12i,m22i - v.m22i,m03i - v.m03i,m13i - v.m13i,m23i - v.m23i)
	def +(s : Float) = new ReadMat3x4(m00i + s,m10i + s,m20i + s,m01i + s,m11i + s,m21i + s,m02i + s,m12i + s,m22i + s,m03i + s,m13i + s,m23i + s)
	def -(s : Float) = new ReadMat3x4(m00i - s,m10i - s,m20i - s,m01i - s,m11i - s,m21i - s,m02i - s,m12i - s,m22i - s,m03i - s,m13i - s,m23i - s)
	def *(s : Float) = new ReadMat3x4(m00i * s,m10i * s,m20i * s,m01i * s,m11i * s,m21i * s,m02i * s,m12i * s,m22i * s,m03i * s,m13i * s,m23i * s)
	def /(s : Float) = new ReadMat3x4(m00i / s,m10i / s,m20i / s,m01i / s,m11i / s,m21i / s,m02i / s,m12i / s,m22i / s,m03i / s,m13i / s,m23i / s)
	def * (other : ReadVec4f) : ReadVec3f = new ReadVec3f(
m00 * other.r + m01 * other.g + m02 * other.b + m03 * other.a,
m10 * other.r + m11 * other.g + m12 * other.b + m13 * other.a,
m20 * other.r + m21 * other.g + m22 * other.b + m23 * other.a
)
	def * (other : ReadMat4x2) : ReadMat3x2 = new ReadMat3x2(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
m20 * other.m00 + m21 * other.m10 + m22 * other.m20 + m23 * other.m30,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31,
m20 * other.m01 + m21 * other.m11 + m22 * other.m21 + m23 * other.m31
)
	def * (other : ReadMat4x3) : ReadMat3x3 = new ReadMat3x3(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
m20 * other.m00 + m21 * other.m10 + m22 * other.m20 + m23 * other.m30,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31,
m20 * other.m01 + m21 * other.m11 + m22 * other.m21 + m23 * other.m31,
m00 * other.m02 + m01 * other.m12 + m02 * other.m22 + m03 * other.m32,
m10 * other.m02 + m11 * other.m12 + m12 * other.m22 + m13 * other.m32,
m20 * other.m02 + m21 * other.m12 + m22 * other.m22 + m23 * other.m32
)
	def * (other : ReadMat4x4) : ReadMat3x4 = new ReadMat3x4(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
m20 * other.m00 + m21 * other.m10 + m22 * other.m20 + m23 * other.m30,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31,
m20 * other.m01 + m21 * other.m11 + m22 * other.m21 + m23 * other.m31,
m00 * other.m02 + m01 * other.m12 + m02 * other.m22 + m03 * other.m32,
m10 * other.m02 + m11 * other.m12 + m12 * other.m22 + m13 * other.m32,
m20 * other.m02 + m21 * other.m12 + m22 * other.m22 + m23 * other.m32,
m00 * other.m03 + m01 * other.m13 + m02 * other.m23 + m03 * other.m33,
m10 * other.m03 + m11 * other.m13 + m12 * other.m23 + m13 * other.m33,
m20 * other.m03 + m21 * other.m13 + m22 * other.m23 + m23 * other.m33
)
	def transformPoint ( p : ReadVec3f ) = ReadVec3f(m00*p.x + m01*p.y + m02*p.z + m03,m10*p.x + m11*p.y + m12*p.z + m13,m20*p.x + m21*p.y + m22*p.z + m23)
	def transformVector ( p : ReadVec3f ) = ReadVec3f(m00*p.x + m01*p.y + m02*p.z,m10*p.x + m11*p.y + m12*p.z,m20*p.x + m21*p.y + m22*p.z)
	def rotateX ( theta : Float ) = {		val sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat
		new ReadMat3x4(
		m00, cos*m10 - sin*m20, sin*m10 + cos*m20,
		m01, cos*m11 - sin*m21, sin*m11 + cos*m21,
		m02, cos*m12 - sin*m22, sin*m12 + cos*m22,
		m03, cos*m13 - sin*m23, sin*m13 + cos*m23
)}
	def rotateY ( theta : Float ) = {		val sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat
		new ReadMat3x4(
		cos*m00 + sin*m20, m10, cos*m20 - sin*m00,
		cos*m01 + sin*m21, m11, cos*m21 - sin*m01,
		cos*m02 + sin*m22, m12, cos*m22 - sin*m02,
		cos*m03 + sin*m23, m13, cos*m23 - sin*m03
)}
	def rotateZ ( theta : Float ) = {		val sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat
		new ReadMat3x4(
		cos*m00 - sin*m10, sin*m00 + cos*m10, m20,
		cos*m01 - sin*m11, sin*m01 + cos*m11, m21,
		cos*m02 - sin*m12, sin*m02 + cos*m12, m22,
		cos*m03 - sin*m13, sin*m03 + cos*m13, m23
)}

	def readExternal ( in : ObjectInput ) {
m00i = in.readFloat
m10i = in.readFloat
m20i = in.readFloat
m01i = in.readFloat
m11i = in.readFloat
m21i = in.readFloat
m02i = in.readFloat
m12i = in.readFloat
m22i = in.readFloat
m03i = in.readFloat
m13i = in.readFloat
m23i = in.readFloat
}

	def writeExternal ( out : ObjectOutput ) {
out.writeFloat(m00i)
out.writeFloat(m10i)
out.writeFloat(m20i)
out.writeFloat(m01i)
out.writeFloat(m11i)
out.writeFloat(m21i)
out.writeFloat(m02i)
out.writeFloat(m12i)
out.writeFloat(m22i)
out.writeFloat(m03i)
out.writeFloat(m13i)
out.writeFloat(m23i)
}
	def m00 = m00i
	protected def m00_= ( s : Float ) { m00i = s }
	def m10 = m10i
	protected def m10_= ( s : Float ) { m10i = s }
	def m20 = m20i
	protected def m20_= ( s : Float ) { m20i = s }
	def m01 = m01i
	protected def m01_= ( s : Float ) { m01i = s }
	def m11 = m11i
	protected def m11_= ( s : Float ) { m11i = s }
	def m21 = m21i
	protected def m21_= ( s : Float ) { m21i = s }
	def m02 = m02i
	protected def m02_= ( s : Float ) { m02i = s }
	def m12 = m12i
	protected def m12_= ( s : Float ) { m12i = s }
	def m22 = m22i
	protected def m22_= ( s : Float ) { m22i = s }
	def m03 = m03i
	protected def m03_= ( s : Float ) { m03i = s }
	def m13 = m13i
	protected def m13_= ( s : Float ) { m13i = s }
	def m23 = m23i
	protected def m23_= ( s : Float ) { m23i = s }
	override def toString = "(" + m00 + "," + m10 + "," + m20 + "," + m01 + "," + m11 + "," + m21 + "," + m02 + "," + m12 + "," + m22 + "," + m03 + "," + m13 + "," + m23+ ")"
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadMat3x4 => m00 == v.m00 && m10 == v.m10 && m20 == v.m20 && m01 == v.m01 && m11 == v.m11 && m21 == v.m21 && m02 == v.m02 && m12 == v.m12 && m22 == v.m22 && m03 == v.m03 && m13 == v.m13 && m23 == v.m23
		case mv : Moddable[ReadMat3x4] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 + m00.hashCode) + m10.hashCode) + m20.hashCode) + m01.hashCode) + m11.hashCode) + m21.hashCode) + m02.hashCode) + m12.hashCode) + m22.hashCode) + m03.hashCode) + m13.hashCode) + m23.hashCode
}
object ReadMat3x4{
	def apply (m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float,m03a : Float,m13a : Float,m23a : Float) = new ReadMat3x4(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float,m03a : Float,m13a : Float,m23a : Float)
	def apply (v : ReadMat3x4) = new ReadMat3x4(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21,v.m02,v.m12,v.m22,v.m03,v.m13,v.m23)
}
