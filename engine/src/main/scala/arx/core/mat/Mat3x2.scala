package arx.core.mat;
import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat3x2 extends ReadMat3x2{
	def this(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m20i = m20a
		m01i = m01a
		m11i = m11a
		m21i = m21a
	}
	override def m00_= ( s : Float ) { m00i = s }
	override def m10_= ( s : Float ) { m10i = s }
	override def m20_= ( s : Float ) { m20i = s }
	override def m01_= ( s : Float ) { m01i = s }
	override def m11_= ( s : Float ) { m11i = s }
	override def m21_= ( s : Float ) { m21i = s }
	def +=(v : ReadMat3x2) { m00i += v.m00;m10i += v.m10;m20i += v.m20;m01i += v.m01;m11i += v.m11;m21i += v.m21}
	def -=(v : ReadMat3x2) { m00i -= v.m00;m10i -= v.m10;m20i -= v.m20;m01i -= v.m01;m11i -= v.m11;m21i -= v.m21}
	override def +(v : ReadMat3x2) = new Mat3x2(m00i + v.m00,m10i + v.m10,m20i + v.m20,m01i + v.m01,m11i + v.m11,m21i + v.m21)
	override def -(v : ReadMat3x2) = new Mat3x2(m00i - v.m00,m10i - v.m10,m20i - v.m20,m01i - v.m01,m11i - v.m11,m21i - v.m21)
	def +=(s : Float) {m00i += s;m10i += s;m20i += s;m01i += s;m11i += s;m21i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m20i -= s;m01i -= s;m11i -= s;m21i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m20i *= s;m01i *= s;m11i *= s;m21i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m20i /= s;m01i /= s;m11i /= s;m21i /= s}
	override def +(s : Float) = new Mat3x2(m00i + s,m10i + s,m20i + s,m01i + s,m11i + s,m21i + s)
	override def -(s : Float) = new Mat3x2(m00i - s,m10i - s,m20i - s,m01i - s,m11i - s,m21i - s)
	override def *(s : Float) = new Mat3x2(m00i * s,m10i * s,m20i * s,m01i * s,m11i * s,m21i * s)
	override def /(s : Float) = new Mat3x2(m00i / s,m10i / s,m20i / s,m01i / s,m11i / s,m21i / s)

}
object Mat3x2{
	def apply (m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float) = new Mat3x2(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float)
	def apply (v : ReadMat3x2) = new Mat3x2(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21)
	implicit def toWriteable (v : ReadMat3x2) = new Mat3x2(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21)
}
