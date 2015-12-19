package arx.graphics

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 1/25/13
 * Time: 6:46 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.vec.ReadVec2i
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.ARBTextureFloat
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL30

import scala.collection.mutable

class Texture {
	var name = 0
	var imageData : Image = Image.Sentinel
	var minFilter : Int = GL_NEAREST
	var magFilter : Int = GL_NEAREST
	var wrap : Int = GL12.GL_CLAMP_TO_EDGE
	var mipmap : Boolean = true
	var internalFormat = GL_RGBA
	var targetFormat = GL_RGBA
	var revision = 0

	var floatTexture = false

	def bind () {
		bind(0)
	}
	def bind(i:Int){
		if ( name == 0 || revision < imageData.revision ) { commitTexture() }
		GL.bindTexture(i,name)
	}

	def commitTexture () {
		revision = imageData.revision

		if ( name == 0 ) {
			name = glGenTextures()
		}

		val toRebind = GL.boundTexture(0)
		if ( ! floatTexture ) {
			GL.bindTexture(name)

			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,minFilter)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,magFilter)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,wrap)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,wrap)


			imageData.data.rewind()
			glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,imageData.width,imageData.height,0,GL_RGBA,GL_UNSIGNED_BYTE,imageData.data)
			if ( mipmap ) {
				GL30.glGenerateMipmap(GL_TEXTURE_2D)
			}
		} else {
			GL.bindTexture(name)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST)

			imageData.data.rewind()
			val floatData = BufferUtils.createFloatBuffer(imageData.data.capacity())
			var i = 0; while ( i < floatData.capacity() ) {
				floatData.put(i, toUnsignedInt(imageData.data.get(i)) / 255.0f)
				i += 1
			}
			floatData.rewind()

			glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,imageData.width,imageData.height,0,ARBTextureFloat.GL_RGBA32F_ARB,GL_FLOAT,floatData)
		}
		GL.bindTexture(toRebind)
	}

	def commitSubTexture (offset : ReadVec2i,dims : ReadVec2i) {
		if ( name == 0 ) { commitTexture() }
		else {
			val toRebind = GL.boundTexture(0)

			GL.bindTexture(name)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,minFilter)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,magFilter)

			imageData.data.rewind()
			glTexSubImage2D(GL_TEXTURE_2D,0,offset.x,offset.y,dims.x,dims.y,GL_RGBA,GL_UNSIGNED_BYTE,imageData.data)
			if ( mipmap ) {
				GL30.glGenerateMipmap(GL_TEXTURE_2D)
			}

			GL.bindTexture(toRebind)
		}
	}
}

object Texture {
	val texturesByImage = new mutable.HashMap[Image,Texture]

	def fromImage (img : Image) = {
		texturesByImage.getOrElseUpdate(img, {
			val t = new Texture
			t.imageData = img
			t
		})
	}
}
