package arx.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 2/12/14
 * Time: 11:46 AM
 */

import java.nio.ByteBuffer

import arx.application.Noto
import arx.core.vec.ReadVec3i
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL30


class Texture3D ( width : Int , height : Int , depth : Int , components : Int ) {
	val dimensions = ReadVec3i(width,height,depth)
	val data = ByteBuffer.allocateDirect(width * height * depth * components)
	var lastUpdated = 0
	var lastSolidified = -1
	var name = 0

	var minFilter = GL_NEAREST
	var magFilter = GL_NEAREST
	var wrap = GL_REPEAT

	private val ZMult = width * height * components
	private val YMult = width * components
	private val XMult = components

	def apply ( x : Int , y : Int , z : Int , c : Int ) = {
		data.get( x * XMult + y * YMult + z * ZMult + c )
	}
	def update ( x : Int , y : Int , z : Int , c : Int , b : Byte ) {
		data.put( x * XMult + y * YMult + z * ZMult + c , b )
	}


	def bind() { bind(0) }
	def bind(i : Int) {
		if ( name == 0 || lastUpdated > lastSolidified ) { commitTexture() }
		GL.bindTexture(i,name)
	}

	def commitTexture () {
		lastSolidified = lastUpdated
		if ( name == 0 ) { name = GL.backing.glGenTextures() }

		if ( GL.checkError() ) { Noto.warn("pre-wat®") }

		val texToRebind = GL.boundTexture(0)
		GL.bindTexture(name)
		glTexParameteri(GL12.GL_TEXTURE_3D,GL_TEXTURE_MIN_FILTER,minFilter)
		glTexParameteri(GL12.GL_TEXTURE_3D,GL_TEXTURE_MAG_FILTER,magFilter)
		glTexParameteri(GL12.GL_TEXTURE_3D,GL_TEXTURE_WRAP_S,wrap)
		glTexParameteri(GL12.GL_TEXTURE_3D,GL_TEXTURE_WRAP_T,wrap)
		glTexParameteri(GL12.GL_TEXTURE_3D,GL12.GL_TEXTURE_WRAP_R,wrap)

		data.rewind()
		val format = components match {
			case 1 => GL_RED
			case 3 => GL_RGB
			case 4 => GL_RGBA
			case _ => Noto.warn(s"Invalid number of components for texture3d, ($components)"); GL_R
		}

		val internalFormat = components match {
			case 1 => GL30.GL_R8
			case 3 => GL_RGB
			case 4 => GL_RGBA
			case _ => Noto.warn(s"Invalid number of components for texture3d, ($components)"); GL_R
		}
		GL12.glTexImage3D(GL12.GL_TEXTURE_3D,0,internalFormat,width,height,depth,0,format,GL_UNSIGNED_BYTE,data)
		if ( GL.checkError() ) { Noto.warn("Error encountered during 3d texture commit") }
		GL.bindTexture(texToRebind)
	}
}