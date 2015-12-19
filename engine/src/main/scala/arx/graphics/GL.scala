package arx.graphics

import java.nio.ByteBuffer
import java.nio.FloatBuffer

import arx.application.Application
import arx.application.Noto
import arx.core.mat.Mat4x4
import arx.core.mat.ReadMat4x4
import arx.core.math.Recti
import arx.core.vec._
import arx.graphics.shader.Shader
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL30

import scala.collection.mutable._
import scala.language.postfixOps
import scala.ref.SoftReference
import scalaxy.loops._

/**
 *
 */

//TODO: Update and cull

object GL {
	val backing : GLBacking = LWJGL3Backing

	val vbosToDestroy = new SynchronizedQueue[Int]()
	val texturesToDestroy = new SynchronizedQueue[Int]()

	var disabled = false

	var viewport = Recti(0,0,1440,900)
	def viewportSize = ReadVec2i(viewport.width,viewport.height)

	var boundTexture = Array(0,0,0,0,0,0,0,0,0,0,0)
	var activeTextureNumber = 0
	var boundFramebuffer = 0

	var debugMode = "true" == System.getProperty("GLDebug","false")

	val ModelViewUniform = "ModelViewMatrix"
	val ProjectionUniform = "ProjectionMatrix"

	def loadIdentityModelViewMatrix (shader : Shader): Unit = {
		val viewMatrix = Mat4x4.Identity
		shader.setUniform(ModelViewUniform,viewMatrix,tolerateAbsence = true)
	}

	def loadIdentityProjectionMatrix (shader : Shader): Unit = {
		val projMatrix = Mat4x4.Identity
		shader.setUniform(ProjectionUniform,projMatrix,tolerateAbsence = true)
	}

	def bindFramebuffer ( fboName : Int ) {
		if ( ! disabled ) {
			if ( boundFramebuffer != fboName ) {
				boundFramebuffer = fboName
				GL30.glBindFramebuffer(GL30.GL_FRAMEBUFFER,fboName)
			}
		}
	}

	def bindTexture ( textureName : Int ) {
		bindTexture(0,textureName)
	}
	def bindTexture ( textureNumber : Int , textureName : Int ) {
		if ( ! disabled ) {
//			println("Binding " + textureName + " -> " + textureNumber)
			if ( activeTextureNumber != textureNumber ) {

				backing.glActiveTexture(GL_TEXTURE0 + textureNumber)
				activeTextureNumber = textureNumber
			}
			if ( textureName != boundTexture(textureNumber) ) {
				backing.glBindTexture(GL_TEXTURE_2D,textureName)
				boundTexture(textureNumber) = textureName
			}
		}
	}

	var activeShader : Int = 0

	def bindShader ( shaderProgram : Int ) {
		if ( activeShader != shaderProgram ) {
			activeShader = shaderProgram
			backing.glUseProgram(shaderProgram)
		}
	}
	def unbindShader () { bindShader(0) }

	def destroyTexture ( texture : TextureBlock ) {
		texturesToDestroy.enqueue(texture.textureID)
	}

	@inline
	protected def enhancedTruth ( setting : Int , truth : Boolean ) = {
		if ( debugMode && (setting == GL_CULL_FACE) ) { false } else { truth }
	}

	val glStates = new HashMap[Int,Stack[Boolean]]
	def glPushState(setting:Int,truth:Boolean) {
		if ( ! disabled ) {
			val stack = glStates.getOrElseUpdate(setting,{ val s = new Stack[Boolean]; s.push(false); s })
			val current = stack.top
			val newTruth = enhancedTruth(setting,truth)
			stack.push(newTruth)
			if ( current != newTruth ) {
				if ( newTruth ) { backing.glEnable(setting) } else { GL11.glDisable(setting) }
			}
		}
	}
	def glPopState (setting:Int) {
		if ( ! disabled ) {
			val stack = glStates.getOrElse(setting,{ val s = new Stack[Boolean]; s.push(false); s })
			val current = stack.top
			stack.pop()
			if ( current != stack.top ) { if ( stack.top ) { GL11.glEnable(setting) } else { GL11.glDisable(setting) } }
		}
	}
	def glSetState ( setting : Int , enable : Boolean ) {
		if ( ! disabled ) {
			val stack = glStates.getOrElseUpdate(setting,{ val s = new Stack[Boolean]; s.push(! enable); s })
			val current = stack.top
			if ( current != enable ) {
				stack.pop()
				val newTruth = enhancedTruth(setting,enable)
				stack.push(newTruth)
				if ( newTruth ) { GL11.glEnable(setting) } else { GL11.glDisable(setting) }
			}
		}
	}

	var curDepthFunc = -1
	def glSetDepthFunc ( func : Int ) {
		if ( ! disabled ) {
			if ( curDepthFunc != func ) {
				backing.glDepthFunc(func)
				curDepthFunc = func
			}
		}
	}

	var cullFace = -1
	def glSetCullFace ( face : Int ) {
		if ( ! disabled ) {
			if ( cullFace != face ) {
				backing.glCullFace(face)
				cullFace = face
			}
		}
	}

	var depthMask = true
	def glDepthMask ( enable : Boolean ) {
		if ( ! disabled ) {
			if ( depthMask != enable ){
				backing.glDepthMask(enable)
				depthMask = enable
			}
		}
	}

	def fetchColorTextureData (textureId : Int, mipmapLevel : Int = 0) = {
		fetchTextureData(textureId,GL_RGBA,4,mipmapLevel)
	}
	def fetchDepthTextureData (textureId : Int, mipmapLevel : Int = 0) = {
		fetchTextureData(textureId,GL_DEPTH_COMPONENT,1,mipmapLevel)
	}

	def fetchTextureData (textureId : Int, format : Int, components : Int, mipmapLevel : Int) = {
		val pre = GL.boundTexture(0)
		GL.bindTexture(textureId)

		val dimBuffer = BufferUtils.createIntBuffer(4)
		val level = mipmapLevel

		backing.glGetTexLevelParameter(GL_TEXTURE_2D,level,GL_TEXTURE_WIDTH,dimBuffer)
		val width = dimBuffer.get(0)
		dimBuffer.rewind()
		backing.glGetTexLevelParameter(GL_TEXTURE_2D,level,GL_TEXTURE_HEIGHT,dimBuffer)
		val height = dimBuffer.get(0)

		val bb = BufferUtils.createFloatBuffer(width * height * components)
		glGetTexImage(GL_TEXTURE_2D,level,format,GL_FLOAT,bb)
		if (GL.checkError()) {Noto.error("GL error hit on getting tex image")}

		val ret = Image.withDimensions(width,height)
		for (x <- 0 until width optimized; y <- 0 until height optimized ; q <- 0 until 4 optimized) {
			val modQ = q % components
			val f = bb.get(y * width * components + x * components + modQ)

			if (q != 3 || components > 1) {
				ret(x,y,q) = (f * 255).toByte
			} else {
				ret(x,y,q) = 255.toByte
			}
		}

		GL.bindTexture(pre)

		ret
	}



	def lookAt ( eye : ReadVec3f , center : ReadVec3f , up : ReadVec3f ) : ReadMat4x4 = {
		val mat = Mat4x4(1.0f)

		val f = (center - eye).normalizeSafe
		var u = up.normalizeSafe
		val s = f.cross(u).normalizeSafe
		u = s cross f

		mat.m00 = s.x
		mat.m10 = s.y
		mat.m20 = s.z

		mat.m01 = u.x
		mat.m11 = u.y
		mat.m21 = u.z

		mat.m02 = -f.x
		mat.m12 = -f.y
		mat.m22 = -f.z

		mat.m30 = -( s dot eye )
		mat.m31 = -( u dot eye )
		mat.m32 = f dot eye

		mat

		/*
		detail::tvec3<T> f = normalize(center - eye);
		detail::tvec3<T> u = normalize(up);
		detail::tvec3<T> s = normalize(cross(f, u));
		u = cross(s, f);

		detail::tmat4x4<T> Result(1);
		Result[0][0] = s.x;
		Result[1][0] = s.y;
		Result[2][0] = s.z;
		Result[0][1] = u.x;
		Result[1][1] = u.y;
		Result[2][1] = u.z;
		Result[0][2] =-f.x;
		Result[1][2] =-f.y;
		Result[2][2] =-f.z;
		Result[3][0] =-dot(s, eye);
		Result[3][1] =-dot(u, eye);
		Result[3][2] = dot(f, eye);
		return Result;

		 */
	}

	def perspective ( fov : Float , aspectRatio : Float, zNear : Float , zFar : Float ) : ReadMat4x4 = {
		val rad = fov * (math.Pi.toFloat / 180.0f)
		val tanHalfFovy = math.tan(rad / 2.0f).toFloat
		val result = Mat4x4(0.0f)
		result.m00 = 1.0f / (aspectRatio * tanHalfFovy)
		result.m11 = 1.0f / tanHalfFovy
		result.m22 = -(zFar + zNear) / (zFar - zNear)
		result.m23 = -1.0f
		result.m32 = -(2.0f * zFar * zNear) / (zFar - zNear)

		result
		/*
		valType tanHalfFovy = tan(rad / valType(2));
		detail::tmat4x4<valType> Result(valType(0));
		Result[0][0] = valType(1) / (aspect * tanHalfFovy);
		Result[1][1] = valType(1) / (tanHalfFovy);
		Result[2][2] = - (zFar + zNear) / (zFar - zNear);
		Result[2][3] = - valType(1);
		Result[3][2] = - (valType(2) * zFar * zNear) / (zFar - zNear);
		return Result;
		 */
	}

	def ortho ( left : Float , right : Float , bottom : Float , top : Float , zNear : Float , zFar : Float ) : ReadMat4x4 = {
		val result = Mat4x4(1.0f)
		
		result.m00 = 2.0f / (right - left)
		result.m11 = 2.0f / (top - bottom)
		result.m22 = -2.0f / (zFar - zNear)
		result.m30 = - (right + left) / (right - left)
		result.m31 = - (top + bottom) / (top - bottom)
		result.m32 = - (zFar + zNear) / (zFar - zNear)
		
		result
/*
		detail::tmat4x4<valType> Result(1);
		Result[0][0] = valType(2) / (right - left);
		Result[1][1] = valType(2) / (top - bottom);
		Result[2][2] = - valType(2) / (zFar - zNear);
		Result[3][0] = - (right + left) / (right - left);
		Result[3][1] = - (top + bottom) / (top - bottom);
		Result[3][2] = - (zFar + zNear) / (zFar - zNear);
		return Result;
 */
	}

	/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		A useful note for future me, the code we worked from apparently does not so much correspond
		with the way that we do our matrix multiplication. To be honest, none of it makes any sense:

		If we supply glsl with (proj * modelview) it behaves incorrectly, so we apparently do not
		operate in the same manner as glsl. (modelview * proj) works properly. This leads me to believe
		that our matrices are row-major order, as opposed to opengl's column major. The code below
		for sending matrices into opengl reaffirms that.

		When we put A * B into a reference matrix mult it acts the same as our code does when supplied
		with B * A. Presumably this is a quirk of the toString, which must print it in the wrong order...
		or something. Who knows.

		So I don't really know what's going on with that, but the old (wrong) mat mult algo we had
		did not satisfy A * Identity == A, so that was a problem. With new (correct) mat mult the
		below should work, it requires swapping both (proj * model) and (inverse * tmp) from the basis
		code, because of the whole ordering thing.
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  */
	def unproject (win : ReadVec3f,model : ReadMat4x4,proj : ReadMat4x4,viewport : ReadVec4f) = {
		val inverse = this.inverse(model * proj)

		var tmp = Vec4f(win.x,win.y,win.z,1.0f)
		tmp(0) = (tmp(0) - viewport(0)) / viewport(2)
		tmp(1) = (tmp(1) - viewport(1)) / viewport(3)
		tmp = tmp * 2.0f - 1.0f

		val obj = tmp * inverse

		Vec3f(obj(0) / obj(3),obj(1) / obj(3),obj(2) / obj(3))
		/*
		(
		detail::tvec3<T> const & win,
		detail::tmat4x4<T> const & model,
		detail::tmat4x4<T> const & proj,
		detail::tvec4<U> const & viewport
	)
	{
		detail::tmat4x4<T> inverse = glm::inverse(proj * model);

		detail::tvec4<T> tmp = detail::tvec4<T>(win, T(1));
		tmp.x = (tmp.x - T(viewport[0])) / T(viewport[2]);
		tmp.y = (tmp.y - T(viewport[1])) / T(viewport[3]);
		tmp = tmp * T(2) - T(1);

		detail::tvec4<T> obj = inverse * tmp;
		obj /= obj.w;

		return detail::tvec3<T>(obj);
		 */
	}


	def determinant ( mat : ReadMat4x4 )  = {
		import mat._
		var f =
			m00 * ((m11 * m22 * m33 + m12 * m23 * m31 + m13 * m21 * m32) -
			m13 * m22 * m31 -
			m11 * m23 * m32 -
			m12 * m21 * m33);
		f -= m01 * ((m10 * m22 * m33 + m12 * m23 * m30 + m13 * m20 * m32) -
			m13 * m22 * m30 -
			m10 * m23 * m32 -
			m12 * m20 * m33);
		f += m02 * ((m10 * m21 * m33 + m11 * m23 * m30 + m13 * m20 * m31) -
			m13 * m21 * m30 -
			m10 * m23 * m31 -
			m11 * m20 * m33);
		f -= m03 * ((m10 * m21 * m32 + m11 * m22 * m30 + m12 * m20 * m31) -
			m12 * m21 * m30 -
			m10 * m22 * m31 -
			m11 * m20 * m32);
		f
		/*
		        public float determinant() {
                float f =
                        m00
                                * ((m11 * m22 * m33 + m12 * m23 * m31 + m13 * m21 * m32)
                                        - m13 * m22 * m31
                                        - m11 * m23 * m32
                                        - m12 * m21 * m33);
                f -= m01
                        * ((m10 * m22 * m33 + m12 * m23 * m30 + m13 * m20 * m32)
                                - m13 * m22 * m30
                                - m10 * m23 * m32
                                - m12 * m20 * m33);
                f += m02
                        * ((m10 * m21 * m33 + m11 * m23 * m30 + m13 * m20 * m31)
                                - m13 * m21 * m30
                                - m10 * m23 * m31
                                - m11 * m20 * m33);
                f -= m03
                        * ((m10 * m21 * m32 + m11 * m22 * m30 + m12 * m20 * m31)
                                - m12 * m21 * m30
                                - m10 * m22 * m31
                                - m11 * m20 * m32);
                return f;
        }
		 */
	}

	private def determinant3x3(t00: Float, t01 : Float, t02 : Float,
		t10 : Float, t11 : Float, t12 : Float,
		t20 : Float, t21 : Float, t22 : Float) =
	{
		t00 * (t11 * t22 - t12 * t21) + t01 * (t12 * t20 - t10 * t22) + t02 * (t10 * t21 - t11 * t20);
	}


	def inverse ( src : ReadMat4x4 ) = {

		val Coef00 = src.m22 * src.m33 - src.m32 * src.m23
		val Coef02 = src.m12 * src.m33 - src.m32 * src.m13
		val Coef03 = src.m12 * src.m23 - src.m22 * src.m13

		val Coef04 = src.m21 * src.m33 - src.m31 * src.m23
		val Coef06 = src.m11 * src.m33 - src.m31 * src.m13
		val Coef07 = src.m11 * src.m23 - src.m21 * src.m13

		val Coef08 = src.m21 * src.m32 - src.m31 * src.m22
		val Coef10 = src.m11 * src.m32 - src.m31 * src.m12
		val Coef11 = src.m11 * src.m22 - src.m21 * src.m12

		val Coef12 = src.m20 * src.m33 - src.m30 * src.m23
		val Coef14 = src.m10 * src.m33 - src.m30 * src.m13
		val Coef15 = src.m10 * src.m23 - src.m20 * src.m13

		val Coef16 = src.m20 * src.m32 - src.m30 * src.m22
		val Coef18 = src.m10 * src.m32 - src.m30 * src.m12
		val Coef19 = src.m10 * src.m22 - src.m20 * src.m12

		val Coef20 = src.m20 * src.m31 - src.m30 * src.m21
		val Coef22 = src.m10 * src.m31 - src.m30 * src.m11
		val Coef23 = src.m10 * src.m21 - src.m20 * src.m11

		val SignA = Vec4f(+1, -1, +1, -1)
		val SignB = Vec4f(-1, +1, -1, +1)

		val Fac0 = Vec4f(Coef00, Coef00, Coef02, Coef03)
		val Fac1 = Vec4f(Coef04, Coef04, Coef06, Coef07)
		val Fac2 = Vec4f(Coef08, Coef08, Coef10, Coef11)
		val Fac3 = Vec4f(Coef12, Coef12, Coef14, Coef15)
		val Fac4 = Vec4f(Coef16, Coef16, Coef18, Coef19)
		val Fac5 = Vec4f(Coef20, Coef20, Coef22, Coef23)

		val Vec0 = Vec4f(src.m10, src.m00, src.m00, src.m00)
		val Vec1 = Vec4f(src.m11, src.m01, src.m01, src.m01)
		val Vec2 = Vec4f(src.m12, src.m02, src.m02, src.m02)
		val Vec3 = Vec4f(src.m13, src.m03, src.m03, src.m03)

		val Inv0 = SignA * (Vec1 * Fac0 - Vec2 * Fac1 + Vec3 * Fac2)
		val Inv1 = SignB * (Vec0 * Fac0 - Vec2 * Fac3 + Vec3 * Fac4)
		val Inv2 = SignA * (Vec0 * Fac1 - Vec1 * Fac3 + Vec3 * Fac5)
		val Inv3 = SignB * (Vec0 * Fac2 - Vec1 * Fac4 + Vec2 * Fac5)

//		val Inverse = Mat4x4(Inv0(0), Inv0(1), Inv0(2), Inv0(3),
//									Inv1(0), Inv1(1), Inv1(2), Inv1(3),
//									Inv2(0), Inv2(1), Inv2(2), Inv2(3),
//									Inv3(0), Inv3(1), Inv3(2), Inv3(3)

		val Inverse = Mat4x4(Inv0(0), Inv1(0), Inv2(0), Inv3(0),
									Inv0(1), Inv1(1), Inv2(1), Inv3(1),
									Inv0(2), Inv1(2), Inv2(2), Inv3(2),
									Inv0(3), Inv1(3), Inv2(3), Inv3(3)
		)

//		val Row0 = Vec4f(Inverse.m00, Inverse.m10, Inverse.m20, Inverse.m30);
		val Row0 = Vec4f(Inverse.m00, Inverse.m01, Inverse.m02, Inverse.m03);

		val Determinant = Vec4f(src.m00,src.m10,src.m20,src.m30) dot Row0

		Inverse /= Determinant;

		Inverse;

		
		/*
		val determinant = this.determinant(src);

		if (determinant != 0) {
			/*
			 * m00 m01 m02 m03
			 * m10 m11 m12 m13
			 * m20 m21 m22 m23
			 * m30 m31 m32 m33
			 */
			val dest = Mat4x4(0.0f)
			val determinant_inv = 1f/determinant;

			// first row
			val t00 =  determinant3x3(src.m11, src.m12, src.m13, src.m21, src.m22, src.m23, src.m31, src.m32, src.m33);
			val t01 = -determinant3x3(src.m10, src.m12, src.m13, src.m20, src.m22, src.m23, src.m30, src.m32, src.m33);
			val t02 =  determinant3x3(src.m10, src.m11, src.m13, src.m20, src.m21, src.m23, src.m30, src.m31, src.m33);
			val t03 = -determinant3x3(src.m10, src.m11, src.m12, src.m20, src.m21, src.m22, src.m30, src.m31, src.m32);

			val t10 = -determinant3x3(src.m01, src.m02, src.m03, src.m21, src.m22, src.m23, src.m31, src.m32, src.m33);
			val t11 =  determinant3x3(src.m00, src.m02, src.m03, src.m20, src.m22, src.m23, src.m30, src.m32, src.m33);
			val t12 = -determinant3x3(src.m00, src.m01, src.m03, src.m20, src.m21, src.m23, src.m30, src.m31, src.m33);
			val t13 =  determinant3x3(src.m00, src.m01, src.m02, src.m20, src.m21, src.m22, src.m30, src.m31, src.m32);

			val t20 =  determinant3x3(src.m01, src.m02, src.m03, src.m11, src.m12, src.m13, src.m31, src.m32, src.m33);
			val t21 = -determinant3x3(src.m00, src.m02, src.m03, src.m10, src.m12, src.m13, src.m30, src.m32, src.m33);
			val t22 =  determinant3x3(src.m00, src.m01, src.m03, src.m10, src.m11, src.m13, src.m30, src.m31, src.m33);
			val t23 = -determinant3x3(src.m00, src.m01, src.m02, src.m10, src.m11, src.m12, src.m30, src.m31, src.m32);

			val t30 = -determinant3x3(src.m01, src.m02, src.m03, src.m11, src.m12, src.m13, src.m21, src.m22, src.m23);
			val t31 =  determinant3x3(src.m00, src.m02, src.m03, src.m10, src.m12, src.m13, src.m20, src.m22, src.m23);
			val t32 = -determinant3x3(src.m00, src.m01, src.m03, src.m10, src.m11, src.m13, src.m20, src.m21, src.m23);
			val t33 =  determinant3x3(src.m00, src.m01, src.m02, src.m10, src.m11, src.m12, src.m20, src.m21, src.m22);

			// transpose and divide by the determinant
			dest.m00 = t00*determinant_inv;
			dest.m11 = t11*determinant_inv;
			dest.m22 = t22*determinant_inv;
			dest.m33 = t33*determinant_inv;
			dest.m01 = t10*determinant_inv;
			dest.m10 = t01*determinant_inv;
			dest.m20 = t02*determinant_inv;
			dest.m02 = t20*determinant_inv;
			dest.m12 = t21*determinant_inv;
			dest.m21 = t12*determinant_inv;
			dest.m03 = t30*determinant_inv;
			dest.m30 = t03*determinant_inv;
			dest.m13 = t31*determinant_inv;
			dest.m31 = t13*determinant_inv;
			dest.m32 = t23*determinant_inv;
			dest.m23 = t32*determinant_inv;

			dest
		} else {
			Noto.warn("Inversion went...poorly")
			Mat4x4(1.0f)
		}
		*/
	}

	def transpose (mat : ReadMat4x4) = {
		val ret = Mat4x4(0.0f)

		ret.m00 = mat.m00
		ret.m01 = mat.m10
		ret.m02 = mat.m20
		ret.m03 = mat.m30

		ret.m10 = mat.m01
		ret.m11 = mat.m11
		ret.m12 = mat.m21
		ret.m13 = mat.m31

		ret.m20 = mat.m02
		ret.m21 = mat.m12
		ret.m22 = mat.m22
		ret.m23 = mat.m32

		ret.m30 = mat.m03
		ret.m31 = mat.m13
		ret.m32 = mat.m23
		ret.m33 = mat.m33

		ret
	}

	/** NOTE : Ordering and correctness with these functions is...uncertain, our matrix math is probably row major
	  * (not entirely certain) but opengl is column major, so those can interact weirdly
	  */
	def translate (mat : ReadMat4x4, x : Float, y : Float, z : Float) = {
		val multBy = Mat4x4.apply(	1.0f,0.0f,0.0f,x,
											0.0f,1.0f,0.0f,y,
											0.0f,0.0f,1.0f,z,
											0.0f,0.0f,0.0f,1.0f )
		multBy * mat
	}

	def scale (mat : ReadMat4x4, x : Float, y : Float, z : Float) = {
		val multBy = Mat4x4.apply(	x   ,0.0f,0.0f,0.0f,
											0.0f,y   ,0.0f,0.0f,
											0.0f,0.0f,z   ,0.0f,
											0.0f,0.0f,0.0f,1.0f )
		multBy * mat
	}

	val matBuf = BufferUtils.createFloatBuffer(16)

	def toTmpBuffer ( mat : ReadMat4x4 ) = {
		val buf = matBuf
		buf.put(mat.m00)
		buf.put(mat.m01)
		buf.put(mat.m02)
		buf.put(mat.m03)
		buf.put(mat.m10)
		buf.put(mat.m11)
		buf.put(mat.m12)
		buf.put(mat.m13)
		buf.put(mat.m20)
		buf.put(mat.m21)
		buf.put(mat.m22)
		buf.put(mat.m23)
		buf.put(mat.m30)
		buf.put(mat.m31)
		buf.put(mat.m32)
		buf.put(mat.m33)
		buf.flip()
		buf
	}

	def toBuffer ( mat : ReadMat4x4 , buf: FloatBuffer ) = {
		buf.put(mat.m00)
		buf.put(mat.m01)
		buf.put(mat.m02)
		buf.put(mat.m03)
		buf.put(mat.m10)
		buf.put(mat.m11)
		buf.put(mat.m12)
		buf.put(mat.m13)
		buf.put(mat.m20)
		buf.put(mat.m21)
		buf.put(mat.m22)
		buf.put(mat.m23)
		buf.put(mat.m30)
		buf.put(mat.m31)
		buf.put(mat.m32)
		buf.put(mat.m33)
		buf.flip()
		buf
	}


	def toTmpBuffer3f ( arr : Vector[ReadVec3f]) = {
		val buf = BufferUtils.createFloatBuffer(arr.size * 3)

		for ( v <- arr ) {
			buf.put(v.x)
			buf.put(v.y)
			buf.put(v.z)
		}

		buf.flip()
		buf
	}

	def toTmpBufferf(arr: Vector[Float]) = {
		val buf = BufferUtils.createFloatBuffer(arr.size)

		for ( v <- arr ) {
			buf.put(v)
		}

		buf.flip()
		buf
	}

	var tick = 0
	var deletionsPerTick = 5
	def update(f: Float) {
		if ( ! disabled ) {
			var countdown = deletionsPerTick
			tick += 1
			while ( countdown > 0 && vbosToDestroy.nonEmpty ) {
				val vid = vbosToDestroy.dequeue()
				if ( vid != 0 ) {
					backing.glDeleteBuffers(vid)
					countdown -= 1
				}
			}
			while ( countdown > 0 && texturesToDestroy.nonEmpty ) {
				val tid = texturesToDestroy.dequeue()
				if ( tid != 0 ) {
					countdown -= 1
					backing.glDeleteTextures(tid)
				}
			}
		}
	}

	def checkError () = {
		val err = backing.glGetError
		if ( err != 0 ) {
			val str = err match {
				case GL11.GL_INVALID_OPERATION => "Invalid operation"
				case GL11.GL_INVALID_ENUM => "Invalid enum"
				case GL11.GL_INVALID_VALUE => "Invalid value"
				case GL11.GL_OUT_OF_MEMORY => "Out of memory"
				case GL30.GL_INVALID_FRAMEBUFFER_OPERATION => "Invalid framebuffer operation"
				case _ => "Unknown error type"
			}

			Noto.error("Opengl error : " + str)
			true
		} else { false }
	}


//	var bufferCache = new ArrayBuffer[SoftReference[ByteBuffer]]()
	val wasteLimit = 0.15f

	var bufCache = Map[Int,List[SoftReference[ByteBuffer]]]()

	def makeNewBuffer ( size : Int ) = BufferUtils.createByteBuffer(size)

	val bumper = BufferUtils.createByteBuffer(1)

	def getBumperBuffer : ByteBuffer = bumper

	var bytesActive : Long = 0

	def createByteBuffer ( size : Int ) : ByteBuffer = {
		val ret = synchronized {
			bufCache.get(size) match {
				case Some(list) =>
					list match {
						case ref :: tail =>
							ref.get match {
								case Some(deref) => {
									bufCache += size -> tail
									deref
								}
								case None => {
									bufCache += size -> tail
									makeNewBuffer(size)
								}
							}
						case Nil =>
							bufCache -= size
							makeNewBuffer(size)
					}
				case None => makeNewBuffer(size)
			}
		}
		locker synchronized {
			bytesActive += ret.capacity()
//			Noto.info("Bytes active increased to : " + bytesActive);
		}
		ret

//		synchronized {
//			var i = 0;
//			while ( i < bufferCache.size ) {
//				val bufRef = bufferCache(i)
//				if ( bufRef != null ) {
//					val buf = bufRef()
//					if ( buf != null ) {
//						val cap = buf.capacity()
//						if ( cap >= size ){
//							if ( cap <= size * (1.0f + wasteLimit) ) {
//								bufferCache(i) = null
//								return buf;
//							} else {
//								//Everything that follows will be larger, just go ahead and create it
//								return makeNewBuffer(size)
//							}
//						}
//					}
//				}
//				i += 1
//			}
//
//			makeNewBuffer(size)
//		}
	}
	val locker = new Object()

	def freeByteBuffer ( buffer : ByteBuffer ) {
		if ( ! (buffer eq bumper) ) {
//			synchronized {
//				var insertBefore = bufferCache.size
//				var i = 0;
//				val bcs = bufferCache.size
//				while ( i < bcs ) {
//					val br = bufferCache(i)
//					if ( br != null ) {
//						val b = br()
//						if ( b != null ) {
//							if ( b.capacity() >= buffer.capacity() ) { insertBefore = i; i = bcs }
//						}
//					}
//					i += 1
//				}
			locker synchronized {
				bytesActive -= buffer.capacity()
//				Noto.info("Bytes active decreased to : " + bytesActive);
			}

			synchronized {
				buffer.position(0)
				buffer.limit(buffer.capacity())
	//				bufferCache.insert(insertBefore,new SoftReference(buffer))
				bufCache += buffer.capacity -> (new SoftReference(buffer) :: bufCache.getOrElse(buffer.capacity,Nil))
			}
//			}
		}
	}


	var elementsDrawnThisFrame : Long = 0.toLong
}