package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 11:51 AM
 * Created by nonvirtualthunk
 */

import java.io.Externalizable
import java.io.File
import java.io.ObjectInput
import java.io.ObjectOutput

import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2f
import arx.graphics.GL
import arx.graphics.Image
import arx.graphics.TextureBlock
import org.lwjgl.opengl.GL11._

//class PixelFont(dir : File) extends TBitmappedFont with Externalizable {
//	val textureBlock : TextureBlock = new TextureBlock(1024,1024)
//	textureBlock.minFilter = GL_LINEAR
//	textureBlock.magFilter = GL_NEAREST
//	textureBlock.borderWidth = 2
//	val texCoordsByAscii = Array.ofDim[ReadVec2f](100,4)
//	var textureSlot = -1
//
//	for ( c <- ('A' to 'Z').toList ::: ('0' to '9').toList ) {
//		val letterImage = Image.loadFromFile(new File(dir,c + ".png").getAbsolutePath)
//		textureBlock.addTexture(letterImage)
//		texCoordsByAscii( c.toInt - 32) = textureBlock.texCoords(letterImage)
//		texCoordsByAscii( c.toLower.toInt - 32) = textureBlock.texCoords(letterImage)
//	}
//	for ( i <- 0 until 100 ) {
//		for ( j <- 0 until 4 ) {
//			if ( texCoordsByAscii(i)(j) == null ) {
//				texCoordsByAscii(i)(j) = Vec2f(0.0f,0.0f)
//			}
//		}
//	}
//
//	def writeExternal(p1: ObjectOutput) { }
//	def readExternal(p1: ObjectInput) { }
//
//	def characterTexCoords(c: Char) = texCoordsByAscii(c.toInt - 32)
//
//	def bind (i: Int) {
//		textureSlot = i
//		textureBlock.bind(textureSlot)
//	}
//	def unbind () {
//		if ( textureSlot >= 0 ) {
//			textureSlot = -1
//			GL.bindTexture(textureSlot,0)
//		}
//	}
//
//	def characterWidth(c: Char) = { throw new Error("Pixel font character widths not yet implemented") }
//	def characterHeight(c: Char) = { throw new Error("Pixel font character widths not yet implemented") }
//
//	def maxCharacterDimensions = throw new IllegalStateException("Not implemented")
//	override def maxCharacterDimensionsPixels: ReadVec2i = ???
//}