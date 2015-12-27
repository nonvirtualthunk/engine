package arx.engine.simple

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/7/15
 * Time: 7:18 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec._
import arx.graphics.AVBO
import arx.graphics.DynamicVBO
import arx.graphics.Image
import arx.graphics.TextureBlock
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import scalaxy.loops._

class Canvas {
	protected val vbo = new AVBO(SimpleAttributeProfile)
	vbo.state.set(DynamicVBO.Clean)
	protected val textureBlock = new TextureBlock(2048, 2048)

	protected val blankTC = textureBlock(ResourceManager.image("default/blank.png"))

	protected[engine] def render(): Unit = {
		textureBlock.bind()

		vbo.bind()
		vbo.solidifyIfNecessary()
		vbo.drawElements(GL11.GL_TRIANGLES, skipPostDraw = true)
	}

	protected[engine] def startDraw() = {
		if (vbo.state.compareAndSet(DynamicVBO.Clean, DynamicVBO.Updating)) {
			vbo.clear()
			true
		} else {
			false
		}
	}

	protected[engine] def finishDraw() = {
		vbo.lastUpdatedMarker = vbo.lastSolidifiedMarker + 1
		if (!vbo.state.compareAndSet(DynamicVBO.Updating, DynamicVBO.Updated)) {
			Noto.error("Unexpected underlying vbo state in canvas")
		}
	}


	def drawQuad(centerX: Float, centerY: Float, width: Float, height: Float, image: Image): Unit = {
		new QuadBuilder()
			.withPosition(centerX, centerY)
			.withDimensions(width, height)
			.withTexture(image)
			.draw()
	}

	def drawQuad(center : ReadVec2f, dimensions : ReadVec2f, image: Image): Unit = {
		new QuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.draw()
	}

	def drawQuad(centerX : Float, centerY : Float, width : Float, height : Float, color : ReadVec4f, image : Image) = {
		new QuadBuilder()
			.withPosition(centerX, centerY)
			.withDimensions(width, height)
			.withTexture(image)
			.withColor(color)
			.draw()
	}

	def drawQuad(center : ReadVec2f, dimensions : ReadVec2f, color : ReadVec4f, image : Image) = {
		new QuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.withColor(color)
			.draw()
	}

	def drawQuad(center : ReadVec3f, dimensions : ReadVec2f, color : ReadVec4f, image : Image) = {
		new QuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.withColor(color)
			.draw()
	}

	def drawQuad(centerX : Float, centerY : Float, width : Float, height : Float, rotationDegrees: Float, color : ReadVec4f, image : Image) = {
		new QuadBuilder()
			.withPosition(centerX, centerY)
			.withDimensions(width, height)
			.withTexture(image)
			.withColor(color)
			.withRotation(rotationDegrees)
			.draw()
	}

	def drawQuad(center : ReadVec2f, dimensions : ReadVec2f, rotationDegrees: Float, color : ReadVec4f, image : Image) = {
		new QuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.withColor(color)
			.withRotation(rotationDegrees)
			.draw()
	}

	def quad(center : ReadVec2f) = new QuadBuilder().withPosition(center)
	def quad(center : ReadVec3f) = new QuadBuilder().withPosition(center)

	protected class QuadBuilder {
		var position = Vec3f.Zero
		var forward = Vec3f.UnitX
		var ortho = Vec3f.UnitY
		var dimensions = Vec2f.One
		var color = Vec4f.One
		var texCoords = blankTC

		def withPosition(x: Float, y: Float, z: Float = 0.0f) = {
			position = Vec3f(x, y, z)
			this
		}

		def withPosition(pos: ReadVec2f) = {
			position = Vec3f(pos.x, pos.y, 0.0f)
			this
		}

		def withPosition(pos: ReadVec3f) = {
			position = pos
			this
		}

		def withForward(vec: ReadVec3f) = {
			forward = vec
			this
		}

		def withOrtho(vec: ReadVec3f) = {
			ortho = vec
			this
		}

		def withRotation(degrees: Float) = {
			forward = Vec3f(cosf(toRad(degrees)), sinf(toRad(degrees)), 0.0f)
			ortho = Vec3f(cosf(toRad(degrees + 90.0f)), sinf(toRad(degrees + 90.0f)), 0.0f)
			this
		}

		def withDimensions(s: Float) = {
			dimensions = Vec2f(s,s)
			this
		}

		def withDimensions(x: Float, y: Float) = {
			dimensions = Vec2f(x, y)
			this
		}

		def withDimensions(d: ReadVec2f) = {
			dimensions = d
			this
		}

		def withColor(r: Float, g: Float, b: Float, a: Float) = {
			color = Vec4f(r, g, b, a)
			this
		}

		def withColor(rgba: ReadVec4f) = {
			color = rgba
			this
		}

		def withTexture(image: Image) : QuadBuilder = {
			texCoords = textureBlock(image)
			this
		}
		def withTexture(imageStr: String) : QuadBuilder = {
			texCoords = textureBlock(ResourceManager.image(imageStr))
			this
		}

		def draw(): Unit = {
			val vi = vbo.incrementVertexOffset(4)
			val ii = vbo.incrementIndexOffset(6)

			var i = 0
			while (i < 4) {
				val x = position.x + forward.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val y = position.y + forward.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val z = position.z + forward.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)

				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + i, x, y, z)
				vbo.setAbf(SimpleAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 255)
				vbo.setA(SimpleAttributeProfile.TexCoordAttribute, vi + i, texCoords(i))
				i += 1
			}
			vbo.setIQuad(ii, vi)
		}
	}

}