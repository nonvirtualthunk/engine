package arx.core.mat;
import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat2x3 extends ReadMat2x3{
	def this(m00a : Float,m10a : Float,m01a : Float,m11a : Float,m02a : Float,m12a : Float){ 
		this()
		m00i = m00a
		m10i = m10a
		m01i = m01a
		m11i = m11a
		m02i = m02a
		m12i = m12a
	}
	override def m00_= ( s : Float ) { m00i = s }
	override def m10_= ( s : Float ) { m10i = s }
	override def m01_= ( s : Float ) { m01i = s }
	override def m11_= ( s : Float ) { m11i = s }
	override def m02_= ( s : Float ) { m02i = s }
	override def m12_= ( s : Float ) { m12i = s }
	def +=(v : ReadMat2x3) { m00i += v.m00;m10i += v.m10;m01i += v.m01;m11i += v.m11;m02i += v.m02;m12i += v.m12}
	def -=(v : ReadMat2x3) { m00i -= v.m00;m10i -= v.m10;m01i -= v.m01;m11i -= v.m11;m02i -= v.m02;m12i -= v.m12}
	override def +(v : ReadMat2x3) = new Mat2x3(m00i + v.m00,m10i + v.m10,m01i + v.m01,m11i + v.m11,m02i + v.m02,m12i + v.m12)
	override def -(v : ReadMat2x3) = new Mat2x3(m00i - v.m00,m10i - v.m10,m01i - v.m01,m11i - v.m11,m02i - v.m02,m12i - v.m12)
	def +=(s : Float) {m00i += s;m10i += s;m01i += s;m11i += s;m02i += s;m12i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m01i -= s;m11i -= s;m02i -= s;m12i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m01i *= s;m11i *= s;m02i *= s;m12i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m01i /= s;m11i /= s;m02i /= s;m12i /= s}
	override def +(s : Float) = new Mat2x3(m00i + s,m10i + s,m01i + s,m11i + s,m02i + s,m12i + s)
	override def -(s : Float) = new Mat2x3(m00i - s,m10i - s,m01i - s,m11i - s,m02i - s,m12i - s)
	override def *(s : Float) = new Mat2x3(m00i * s,m10i * s,m01i * s,m11i * s,m02i * s,m12i * s)
	override def /(s : Float) = new Mat2x3(m00i / s,m10i / s,m01i / s,m11i / s,m02i / s,m12i / s)

}
object Mat2x3{
	def apply (m00a : Float,m10a : Float,m01a : Float,m11a : Float,m02a : Float,m12a : Float) = new Mat2x3(m00a : Float,m10a : Float,m01a : Float,m11a : Float,m02a : Float,m12a : Float)
	def apply (v : ReadMat2x3) = new Mat2x3(v.m00,v.m10,v.m01,v.m11,v.m02,v.m12)
	implicit def toWriteable (v : ReadMat2x3) = new Mat2x3(v.m00,v.m10,v.m01,v.m11,v.m02,v.m12)
}
