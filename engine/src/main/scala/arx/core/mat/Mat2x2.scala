package arx.core.mat;
import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat2x2 extends ReadMat2x2{
	def this(m00a : Float,m10a : Float,m01a : Float,m11a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m01i = m01a
		m11i = m11a
	}
	override def m00_= ( s : Float ) { m00i = s }
	override def m10_= ( s : Float ) { m10i = s }
	override def m01_= ( s : Float ) { m01i = s }
	override def m11_= ( s : Float ) { m11i = s }
	def +=(v : ReadMat2x2) { m00i += v.m00;m10i += v.m10;m01i += v.m01;m11i += v.m11}
	def -=(v : ReadMat2x2) { m00i -= v.m00;m10i -= v.m10;m01i -= v.m01;m11i -= v.m11}
	override def +(v : ReadMat2x2) = new Mat2x2(m00i + v.m00,m10i + v.m10,m01i + v.m01,m11i + v.m11)
	override def -(v : ReadMat2x2) = new Mat2x2(m00i - v.m00,m10i - v.m10,m01i - v.m01,m11i - v.m11)
	def +=(s : Float) {m00i += s;m10i += s;m01i += s;m11i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m01i -= s;m11i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m01i *= s;m11i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m01i /= s;m11i /= s}
	override def +(s : Float) = new Mat2x2(m00i + s,m10i + s,m01i + s,m11i + s)
	override def -(s : Float) = new Mat2x2(m00i - s,m10i - s,m01i - s,m11i - s)
	override def *(s : Float) = new Mat2x2(m00i * s,m10i * s,m01i * s,m11i * s)
	override def /(s : Float) = new Mat2x2(m00i / s,m10i / s,m01i / s,m11i / s)

}
object Mat2x2{
	def apply (m00a : Float,m10a : Float,m01a : Float,m11a : Float) = new Mat2x2(m00a : Float,m10a : Float,m01a : Float,m11a : Float)
	def apply (v : ReadMat2x2) = new Mat2x2(v.m00,v.m10,v.m01,v.m11)
	implicit def toWriteable (v : ReadMat2x2) = new Mat2x2(v.m00,v.m10,v.m01,v.m11)
	object _Identity extends ReadMat2x2(1,0,0,1) {
		override def * ( v : ReadMat2x2 ) = v
		def * ( v : Mat2x2 ) = v
	}
	val Identity : ReadMat2x2 = _Identity
}
