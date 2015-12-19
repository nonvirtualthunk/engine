package arx.core.mat;
import arx.core.vec._
import arx.core.Moddable
import arx.core.SelfModdable
import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

@SerialVersionUID(9223372036854770000L)
class ReadMat2x4 extends Externalizable with SelfModdable[ReadMat2x4]{
	def this(m00a : Float,m10a : Float,m01a : Float,m11a : Float,m02a : Float,m12a : Float,m03a : Float,m13a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m01i = m01a
		m11i = m11a
		m02i = m02a
		m12i = m12a
		m03i = m03a
		m13i = m13a
	}
protected var m00i : Float = 0.0f
protected var m10i : Float = 0.0f
protected var m01i : Float = 0.0f
protected var m11i : Float = 0.0f
protected var m02i : Float = 0.0f
protected var m12i : Float = 0.0f
protected var m03i : Float = 0.0f
protected var m13i : Float = 0.0f
	def +(m : Moddable[ReadMat2x4]) = { val v = m.resolve(); new ReadMat2x4(m00i + v.m00i,m10i + v.m10i,m01i + v.m01i,m11i + v.m11i,m02i + v.m02i,m12i + v.m12i,m03i + v.m03i,m13i + v.m13i) }
	def +(v : ReadMat2x4) = new ReadMat2x4(m00i + v.m00i,m10i + v.m10i,m01i + v.m01i,m11i + v.m11i,m02i + v.m02i,m12i + v.m12i,m03i + v.m03i,m13i + v.m13i)
	def -(m : Moddable[ReadMat2x4]) = { val v = m.resolve(); new ReadMat2x4(m00i - v.m00i,m10i - v.m10i,m01i - v.m01i,m11i - v.m11i,m02i - v.m02i,m12i - v.m12i,m03i - v.m03i,m13i - v.m13i) }
	def -(v : ReadMat2x4) = new ReadMat2x4(m00i - v.m00i,m10i - v.m10i,m01i - v.m01i,m11i - v.m11i,m02i - v.m02i,m12i - v.m12i,m03i - v.m03i,m13i - v.m13i)
	def +(s : Float) = new ReadMat2x4(m00i + s,m10i + s,m01i + s,m11i + s,m02i + s,m12i + s,m03i + s,m13i + s)
	def -(s : Float) = new ReadMat2x4(m00i - s,m10i - s,m01i - s,m11i - s,m02i - s,m12i - s,m03i - s,m13i - s)
	def *(s : Float) = new ReadMat2x4(m00i * s,m10i * s,m01i * s,m11i * s,m02i * s,m12i * s,m03i * s,m13i * s)
	def /(s : Float) = new ReadMat2x4(m00i / s,m10i / s,m01i / s,m11i / s,m02i / s,m12i / s,m03i / s,m13i / s)
	def * (other : ReadVec4f) : ReadVec2f = new ReadVec2f(
m00 * other.r + m01 * other.g + m02 * other.b + m03 * other.a,
m10 * other.r + m11 * other.g + m12 * other.b + m13 * other.a
)
	def * (other : ReadMat4x2) : ReadMat2x2 = new ReadMat2x2(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31
)
	def * (other : ReadMat4x3) : ReadMat2x3 = new ReadMat2x3(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31,
m00 * other.m02 + m01 * other.m12 + m02 * other.m22 + m03 * other.m32,
m10 * other.m02 + m11 * other.m12 + m12 * other.m22 + m13 * other.m32
)
	def * (other : ReadMat4x4) : ReadMat2x4 = new ReadMat2x4(
m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31,
m00 * other.m02 + m01 * other.m12 + m02 * other.m22 + m03 * other.m32,
m10 * other.m02 + m11 * other.m12 + m12 * other.m22 + m13 * other.m32,
m00 * other.m03 + m01 * other.m13 + m02 * other.m23 + m03 * other.m33,
m10 * other.m03 + m11 * other.m13 + m12 * other.m23 + m13 * other.m33
)

	def readExternal ( in : ObjectInput ) {
m00i = in.readFloat
m10i = in.readFloat
m01i = in.readFloat
m11i = in.readFloat
m02i = in.readFloat
m12i = in.readFloat
m03i = in.readFloat
m13i = in.readFloat
}

	def writeExternal ( out : ObjectOutput ) {
out.writeFloat(m00i)
out.writeFloat(m10i)
out.writeFloat(m01i)
out.writeFloat(m11i)
out.writeFloat(m02i)
out.writeFloat(m12i)
out.writeFloat(m03i)
out.writeFloat(m13i)
}
	def m00 = m00i
	protected def m00_= ( s : Float ) { m00i = s }
	def m10 = m10i
	protected def m10_= ( s : Float ) { m10i = s }
	def m01 = m01i
	protected def m01_= ( s : Float ) { m01i = s }
	def m11 = m11i
	protected def m11_= ( s : Float ) { m11i = s }
	def m02 = m02i
	protected def m02_= ( s : Float ) { m02i = s }
	def m12 = m12i
	protected def m12_= ( s : Float ) { m12i = s }
	def m03 = m03i
	protected def m03_= ( s : Float ) { m03i = s }
	def m13 = m13i
	protected def m13_= ( s : Float ) { m13i = s }
	override def toString = "(" + m00 + "," + m10 + "," + m01 + "," + m11 + "," + m02 + "," + m12 + "," + m03 + "," + m13+ ")"
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadMat2x4 => m00 == v.m00 && m10 == v.m10 && m01 == v.m01 && m11 == v.m11 && m02 == v.m02 && m12 == v.m12 && m03 == v.m03 && m13 == v.m13
		case mv : Moddable[ReadMat2x4] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 * (41 * (41 * (41 * (41 + m00.hashCode) + m10.hashCode) + m01.hashCode) + m11.hashCode) + m02.hashCode) + m12.hashCode) + m03.hashCode) + m13.hashCode
}
object ReadMat2x4{
	def apply (m00a : Float,m10a : Float,m01a : Float,m11a : Float,m02a : Float,m12a : Float,m03a : Float,m13a : Float) = new ReadMat2x4(m00a : Float,m10a : Float,m01a : Float,m11a : Float,m02a : Float,m12a : Float,m03a : Float,m13a : Float)
	def apply (v : ReadMat2x4) = new ReadMat2x4(v.m00,v.m10,v.m01,v.m11,v.m02,v.m12,v.m03,v.m13)
}
