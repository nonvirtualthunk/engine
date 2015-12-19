package arx.core.mat;
import arx.core.vec._
@SerialVersionUID(9223372036854770000L)
class Mat3x3 extends ReadMat3x3{
	def this(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float){ 
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
	def +=(v : ReadMat3x3) { m00i += v.m00;m10i += v.m10;m20i += v.m20;m01i += v.m01;m11i += v.m11;m21i += v.m21;m02i += v.m02;m12i += v.m12;m22i += v.m22}
	def -=(v : ReadMat3x3) { m00i -= v.m00;m10i -= v.m10;m20i -= v.m20;m01i -= v.m01;m11i -= v.m11;m21i -= v.m21;m02i -= v.m02;m12i -= v.m12;m22i -= v.m22}
	override def +(v : ReadMat3x3) = new Mat3x3(m00i + v.m00,m10i + v.m10,m20i + v.m20,m01i + v.m01,m11i + v.m11,m21i + v.m21,m02i + v.m02,m12i + v.m12,m22i + v.m22)
	override def -(v : ReadMat3x3) = new Mat3x3(m00i - v.m00,m10i - v.m10,m20i - v.m20,m01i - v.m01,m11i - v.m11,m21i - v.m21,m02i - v.m02,m12i - v.m12,m22i - v.m22)
	def +=(s : Float) {m00i += s;m10i += s;m20i += s;m01i += s;m11i += s;m21i += s;m02i += s;m12i += s;m22i += s}
	def -=(s : Float) {m00i -= s;m10i -= s;m20i -= s;m01i -= s;m11i -= s;m21i -= s;m02i -= s;m12i -= s;m22i -= s}
	def *=(s : Float) {m00i *= s;m10i *= s;m20i *= s;m01i *= s;m11i *= s;m21i *= s;m02i *= s;m12i *= s;m22i *= s}
	def /=(s : Float) {m00i /= s;m10i /= s;m20i /= s;m01i /= s;m11i /= s;m21i /= s;m02i /= s;m12i /= s;m22i /= s}
	override def +(s : Float) = new Mat3x3(m00i + s,m10i + s,m20i + s,m01i + s,m11i + s,m21i + s,m02i + s,m12i + s,m22i + s)
	override def -(s : Float) = new Mat3x3(m00i - s,m10i - s,m20i - s,m01i - s,m11i - s,m21i - s,m02i - s,m12i - s,m22i - s)
	override def *(s : Float) = new Mat3x3(m00i * s,m10i * s,m20i * s,m01i * s,m11i * s,m21i * s,m02i * s,m12i * s,m22i * s)
	override def /(s : Float) = new Mat3x3(m00i / s,m10i / s,m20i / s,m01i / s,m11i / s,m21i / s,m02i / s,m12i / s,m22i / s)

}
object Mat3x3{
	def apply (m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float) = new Mat3x3(m00a : Float,m10a : Float,m20a : Float,m01a : Float,m11a : Float,m21a : Float,m02a : Float,m12a : Float,m22a : Float)
	def apply (v : ReadMat3x3) = new Mat3x3(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21,v.m02,v.m12,v.m22)
	implicit def toWriteable (v : ReadMat3x3) = new Mat3x3(v.m00,v.m10,v.m20,v.m01,v.m11,v.m21,v.m02,v.m12,v.m22)
	object _Identity extends ReadMat3x3(1,0,0,0,1,0,0,0,1) {
		override def * ( v : ReadMat3x3 ) = v
		def * ( v : Mat3x3 ) = v
	}
	val Identity : ReadMat3x3 = _Identity
	def apply ( V0 : ReadVec3f , V1 : ReadVec3f , V2 : ReadVec3f ) = {
		new ReadMat3x3( V0.x,V0.y,V0.z, V1.x,V1.y,V1.z,V2.x,V2.y,V2.z)
	}
}
