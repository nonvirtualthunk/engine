package arx.core.mat;
import arx.core.vec._
import arx.core.Moddable
import arx.core.SelfModdable
import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

@SerialVersionUID(9223372036854770000L)
class ReadMat2x2 extends Externalizable with SelfModdable[ReadMat2x2]{
	def this(m00a : Float,m10a : Float,m01a : Float,m11a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m01i = m01a
		m11i = m11a
	}
protected var m00i : Float = 0.0f
protected var m10i : Float = 0.0f
protected var m01i : Float = 0.0f
protected var m11i : Float = 0.0f
	def +(m : Moddable[ReadMat2x2]) = { val v = m.resolve(); new ReadMat2x2(m00i + v.m00i,m10i + v.m10i,m01i + v.m01i,m11i + v.m11i) }
	def +(v : ReadMat2x2) = new ReadMat2x2(m00i + v.m00i,m10i + v.m10i,m01i + v.m01i,m11i + v.m11i)
	def -(m : Moddable[ReadMat2x2]) = { val v = m.resolve(); new ReadMat2x2(m00i - v.m00i,m10i - v.m10i,m01i - v.m01i,m11i - v.m11i) }
	def -(v : ReadMat2x2) = new ReadMat2x2(m00i - v.m00i,m10i - v.m10i,m01i - v.m01i,m11i - v.m11i)
	def +(s : Float) = new ReadMat2x2(m00i + s,m10i + s,m01i + s,m11i + s)
	def -(s : Float) = new ReadMat2x2(m00i - s,m10i - s,m01i - s,m11i - s)
	def *(s : Float) = new ReadMat2x2(m00i * s,m10i * s,m01i * s,m11i * s)
	def /(s : Float) = new ReadMat2x2(m00i / s,m10i / s,m01i / s,m11i / s)
	def * (other : ReadVec2f) : ReadVec2f = new ReadVec2f(
m00 * other.x + m01 * other.y,
m10 * other.x + m11 * other.y
)
	def * (other : ReadMat2x2) : ReadMat2x2 = new ReadMat2x2(
m00 * other.m00 + m01 * other.m10,
m10 * other.m00 + m11 * other.m10,
m00 * other.m01 + m01 * other.m11,
m10 * other.m01 + m11 * other.m11
)
	def * (other : ReadMat2x3) : ReadMat2x3 = new ReadMat2x3(
m00 * other.m00 + m01 * other.m10,
m10 * other.m00 + m11 * other.m10,
m00 * other.m01 + m01 * other.m11,
m10 * other.m01 + m11 * other.m11,
m00 * other.m02 + m01 * other.m12,
m10 * other.m02 + m11 * other.m12
)
	def * (other : ReadMat2x4) : ReadMat2x4 = new ReadMat2x4(
m00 * other.m00 + m01 * other.m10,
m10 * other.m00 + m11 * other.m10,
m00 * other.m01 + m01 * other.m11,
m10 * other.m01 + m11 * other.m11,
m00 * other.m02 + m01 * other.m12,
m10 * other.m02 + m11 * other.m12,
m00 * other.m03 + m01 * other.m13,
m10 * other.m03 + m11 * other.m13
)

	def readExternal ( in : ObjectInput ) {
m00i = in.readFloat
m10i = in.readFloat
m01i = in.readFloat
m11i = in.readFloat
}

	def writeExternal ( out : ObjectOutput ) {
out.writeFloat(m00i)
out.writeFloat(m10i)
out.writeFloat(m01i)
out.writeFloat(m11i)
}
	def m00 = m00i
	protected def m00_= ( s : Float ) { m00i = s }
	def m10 = m10i
	protected def m10_= ( s : Float ) { m10i = s }
	def m01 = m01i
	protected def m01_= ( s : Float ) { m01i = s }
	def m11 = m11i
	protected def m11_= ( s : Float ) { m11i = s }
	override def toString = "(" + m00 + "," + m10 + "," + m01 + "," + m11+ ")"
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadMat2x2 => m00 == v.m00 && m10 == v.m10 && m01 == v.m01 && m11 == v.m11
		case mv : Moddable[ReadMat2x2] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 + m00.hashCode) + m10.hashCode) + m01.hashCode) + m11.hashCode
}
object ReadMat2x2{
	def apply (m00a : Float,m10a : Float,m01a : Float,m11a : Float) = new ReadMat2x2(m00a : Float,m10a : Float,m01a : Float,m11a : Float)
	def apply (v : ReadMat2x2) = new ReadMat2x2(v.m00,v.m10,v.m01,v.m11)
}
