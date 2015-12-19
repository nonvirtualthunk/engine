package arx.core.mat;
import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat3x4 extends ReadMat3x4{
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
	override def m00_= ( s : Float ) { m00i = s }
	override def m10_= ( s : Float ) { m10i = s }
	override def m20_= ( s : Float ) { m20i = s }
	override def m01_= ( s : Float ) { m01i = s }
	override def m11_= ( s : Float ) { m11i = s }
	override def m21_= ( s : Float ) { m21i = s }
	override def m02_= ( s : Float ) { m02i = s }
	override def m12_= ( s : Float ) { m12i = s }
	override def m22_= ( s : Float ) { m22i = s }
	override def m03_= ( s : Float ) { m03i = s }
	override def m13_= ( s : Float ) { m13i = s }
	override def m23_= ( s : Float ) { m23i = s }
	def +=(v : ReadMat3x4) { m00i += v.m00;m10i += v.m10;m20i += v.m20;m01i += v.m01;m11i += v.m11;m21i += v.m21;m02i += v.m02;m12i += v.m12;m22i += v.m22;m03i += v.m03;m13i += v.m13;m23i += v.m23}
	def -=(v : ReadMat3x4) { m00i -= v.m00;m10i -= v.m10;m20i -= v.m20;m01i -= v.m01;m11i -= v.m11;m21i -= v.m21;m02i -= v.m02;m12i -= v.m12;m22i -= v.m22;m03i -= v.m03;m13i -= v.m13;m23i -= v.m23}
	override def +(v : ReadMat3x4) = new Mat3x4(m00i + v.m00,m10i + v.m10,m20i + v.m20,m01i + v.m01,m11i + v.m11,m21i + v.m21,m02i + v.m02,m12i + v.m12,m22i + v.m22,m03i + v.m03,m13i + v.m13,m23i + v.m23)
	override def -(v : ReadMat3x4) = new Mat3x4(m00i - v.m00,m10i - v.m10,m20i - v.m20,m01i - v.m01,m11i - v.m11,m21i - v.m21,m02i - v.m02,m12i - v.m12,m22i - v.m22,m03i - v.m03,m13i - v.m13,m23i - v.m23)
	def +=(s : Float) {m00i += s;m10i += s;m20i += s;m01i += s;m11i += s;m21i += s;m02i += s;m12i += s;m22i += s;m03i += s;m13i += s;m23i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m20i -= s;m01i -= s;m11i -= s;m21i -= s;m02i -= s;m12i -= s;m22i -= s;m03i -= s;m13i -= s;m23i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m20i *= s;m01i *= s;m11i *= s;m21i *= s;m02i *= s;m12i *= s;m22i *= s;m03i *= s;m13i *= s;m23i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m20i /= s;m01i /= s;m11i /= s;m21i /= s;m02i /= s;m12i /= s;m22i /= s;m03i /= s;m13i /= s;m23i /= s}
	override def +(s : Float) = new Mat3x4(m00i + s,m10i + s,m20i + s,m01i + s,m11i + s,m21i + s,m02i + s,m12i + s,m22i + s,m03i + s,m13i + s,m23i + s)
	override def -(s : Float) = new Mat3x4(m00i - s,m10i - s,m20i - s,m01i - s,m11i - s,m21i - s,m02i - s,m12i - s,m22i - s,m03i - s,m13i - s,m23i - s)
	override def *(s : Float) = new Mat3x4(m00i * s,m10i * s,m20i * s,m01i * s,m11i * s,m21i * s,m02i * s,m12i * s,m22i * s,m03i * s,m13i * s,m23i * s)
	override def /(s : Float) = new Mat3x4(m00i / s,m10i / s,m20i / s,m01i / s,m11i / s,m21i / s,m02i / s,m12i / s,m22i / s,m03i / s,m13i / s,m23i / s)

}
object Mat3x4{
	def apply (m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float,m03a : Float,m13a : Float,m23a : Float) = new Mat3x4(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float,m03a : Float,m13a : Float,m23a : Float)
	def apply (v : ReadMat3x4) = new Mat3x4(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21,v.m02,v.m12,v.m22,v.m03,v.m13,v.m23)
	implicit def toWriteable (v : ReadMat3x4) = new Mat3x4(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21,v.m02,v.m12,v.m22,v.m03,v.m13,v.m23)
	def rotateX ( theta : Float ) = {		val sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat
		new ReadMat3x4(
		1, 0, 0,
		0, cos, sin,
		0, -sin, cos,
		0, 0, 0
)}
	def rotateY ( theta : Float ) = {		val sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat
		new ReadMat3x4(
		cos, 0, -sin,
		0, 1, 0,
		sin, 0, cos,
		0, 0, 0
)}
	def rotateZ ( theta : Float ) = {		val sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat
		new ReadMat3x4(
		cos, sin, 0,
		-sin, cos, 0,
		0, 0, 1,
		0, 0, 0
)}
}
