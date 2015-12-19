package arx.core.mat;
import arx.core.vec._
import arx.core.Moddable
import arx.core.SelfModdable
import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput

@SerialVersionUID(9223372036854770000L)
class ReadMat3x2 extends Externalizable with SelfModdable[ReadMat3x2]{
	def this(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m20i = m20a
		m01i = m01a
		m11i = m11a
		m21i = m21a
	}
protected var m00i : Float = 0.0f
protected var m10i : Float = 0.0f
protected var m20i : Float = 0.0f
protected var m01i : Float = 0.0f
protected var m11i : Float = 0.0f
protected var m21i : Float = 0.0f
	def +(m : Moddable[ReadMat3x2]) = { val v = m.resolve(); new ReadMat3x2(m00i + v.m00i,m10i + v.m10i,m20i + v.m20i,m01i + v.m01i,m11i + v.m11i,m21i + v.m21i) }
	def +(v : ReadMat3x2) = new ReadMat3x2(m00i + v.m00i,m10i + v.m10i,m20i + v.m20i,m01i + v.m01i,m11i + v.m11i,m21i + v.m21i)
	def -(m : Moddable[ReadMat3x2]) = { val v = m.resolve(); new ReadMat3x2(m00i - v.m00i,m10i - v.m10i,m20i - v.m20i,m01i - v.m01i,m11i - v.m11i,m21i - v.m21i) }
	def -(v : ReadMat3x2) = new ReadMat3x2(m00i - v.m00i,m10i - v.m10i,m20i - v.m20i,m01i - v.m01i,m11i - v.m11i,m21i - v.m21i)
	def +(s : Float) = new ReadMat3x2(m00i + s,m10i + s,m20i + s,m01i + s,m11i + s,m21i + s)
	def -(s : Float) = new ReadMat3x2(m00i - s,m10i - s,m20i - s,m01i - s,m11i - s,m21i - s)
	def *(s : Float) = new ReadMat3x2(m00i * s,m10i * s,m20i * s,m01i * s,m11i * s,m21i * s)
	def /(s : Float) = new ReadMat3x2(m00i / s,m10i / s,m20i / s,m01i / s,m11i / s,m21i / s)
	def * (other : ReadVec2f) : ReadVec3f = new ReadVec3f(
m00 * other.x + m01 * other.y,
m10 * other.x + m11 * other.y,
m20 * other.x + m21 * other.y
)
	def * (other : ReadMat2x2) : ReadMat3x2 = new ReadMat3x2(
m00 * other.m00 + m01 * other.m10,
m10 * other.m00 + m11 * other.m10,
m20 * other.m00 + m21 * other.m10,
m00 * other.m01 + m01 * other.m11,
m10 * other.m01 + m11 * other.m11,
m20 * other.m01 + m21 * other.m11
)
	def * (other : ReadMat2x3) : ReadMat3x3 = new ReadMat3x3(
m00 * other.m00 + m01 * other.m10,
m10 * other.m00 + m11 * other.m10,
m20 * other.m00 + m21 * other.m10,
m00 * other.m01 + m01 * other.m11,
m10 * other.m01 + m11 * other.m11,
m20 * other.m01 + m21 * other.m11,
m00 * other.m02 + m01 * other.m12,
m10 * other.m02 + m11 * other.m12,
m20 * other.m02 + m21 * other.m12
)
	def * (other : ReadMat2x4) : ReadMat3x4 = new ReadMat3x4(
m00 * other.m00 + m01 * other.m10,
m10 * other.m00 + m11 * other.m10,
m20 * other.m00 + m21 * other.m10,
m00 * other.m01 + m01 * other.m11,
m10 * other.m01 + m11 * other.m11,
m20 * other.m01 + m21 * other.m11,
m00 * other.m02 + m01 * other.m12,
m10 * other.m02 + m11 * other.m12,
m20 * other.m02 + m21 * other.m12,
m00 * other.m03 + m01 * other.m13,
m10 * other.m03 + m11 * other.m13,
m20 * other.m03 + m21 * other.m13
)

	def readExternal ( in : ObjectInput ) {
m00i = in.readFloat
m10i = in.readFloat
m20i = in.readFloat
m01i = in.readFloat
m11i = in.readFloat
m21i = in.readFloat
}

	def writeExternal ( out : ObjectOutput ) {
out.writeFloat(m00i)
out.writeFloat(m10i)
out.writeFloat(m20i)
out.writeFloat(m01i)
out.writeFloat(m11i)
out.writeFloat(m21i)
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
	override def toString = "(" + m00 + "," + m10 + "," + m20 + "," + m01 + "," + m11 + "," + m21+ ")"
	def resolve = this
	def baseValue = this
	override def equals ( other : Any ) = other match {
		case v : ReadMat3x2 => m00 == v.m00 && m10 == v.m10 && m20 == v.m20 && m01 == v.m01 && m11 == v.m11 && m21 == v.m21
		case mv : Moddable[ReadMat3x2] => this == mv.resolve()
		case _ => false
	}
	override def hashCode = 41 * (41 * (41 * (41 * (41 * (41 + m00.hashCode) + m10.hashCode) + m20.hashCode) + m01.hashCode) + m11.hashCode) + m21.hashCode
}
object ReadMat3x2{
	def apply (m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float) = new ReadMat3x2(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float)
	def apply (v : ReadMat3x2) = new ReadMat3x2(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21)
}
