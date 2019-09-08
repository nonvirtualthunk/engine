package arx.engine.simple

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/7/15
  * Time: 7:18 AM
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.math.Rectf
import arx.core.math.Recti
import arx.core.vec._
import arx.graphics.AVBO
import arx.graphics.AttributeProfile
import arx.graphics.Image
import arx.graphics.TextureBlock
import arx.graphics.VBO
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15

abstract class CustomCanvas[QuadBuilderType <: TQuadBuilder](attributeProfile: AttributeProfile) {
	protected val vbo = new AVBO(attributeProfile)
	vbo.state.set(VBO.Clean)
	val textureBlock = new TextureBlock(2048, 2048)

	protected val blankTC = textureBlock(ResourceManager.image("default/blank.png"))

	var viewportOverride = none[(Recti) => Recti]
	protected var originalViewport = Recti(0, 0, 1, 1)

	protected[engine] def render(): Unit = {
		textureBlock.bind()

		vbo.bind()
		vbo.solidifyIfNecessary(GL15.GL_DYNAMIC_DRAW)
		vbo.drawElements(GL11.GL_TRIANGLES, skipPostDraw = true)
	}

	protected[engine] def startDraw() = {
		if (vbo.state.compareAndSet(VBO.Clean, VBO.Updating)) {
			vbo.clear()
			true
		} else {
			false
		}
	}

	protected[engine] def finishDraw() = {
		if (!vbo.state.compareAndSet(VBO.Updating, VBO.Updated)) {
			Noto.error("Unexpected underlying vbo state in canvas")
		}
	}

	def createQuadBuilder(): QuadBuilderType

	def useTexFilters(minFilter: Int, magFilter: Int): Unit = {
		textureBlock.magFilter = magFilter
		textureBlock.minFilter = minFilter
	}

	def useHighCapacity(truth: Boolean): Unit = {
		if (truth) {
			vbo.useIntIndices()
		} else {
			Noto.error("Cannot actually disable int indices right now")
		}
	}
}

class Canvas extends CustomCanvas[QuadBuilder](SimpleAttributeProfile) {
	override def createQuadBuilder(): QuadBuilder = new QuadBuilder(vbo, textureBlock, blankTC)


	def drawLine(start: ReadVec2f, end: ReadVec2f, thickness: Float, color: ReadVec4f): Unit = {
		val middle = (start + end) * 0.5f
		val delta = (end - start).normalizeSafe
		val normal = functions.cross(Vec3f(delta, 0.0f), Vec3f(0.0f, 0.0f, 1.0f))

		createQuadBuilder()
			.withPosition(middle)
			.withDimensions((end - start).lengthSafe, thickness)
			.withOrtho(normal)
			.withForward(Vec3f(delta, 0.0f))
			.withTexture(ResourceManager.blankImage)
			.withColor(color)
			.draw()
	}

	def quad(center: ReadVec2f) = createQuadBuilder().withPosition(center)

	def quad(center: ReadVec3f) = createQuadBuilder().withPosition(center)

	def drawQuad(centerX: Float, centerY: Float, width: Float, height: Float, image: Image): Unit = {
		createQuadBuilder()
			.withPosition(centerX, centerY)
			.withDimensions(width, height)
			.withTexture(image)
			.draw()
	}

	def drawQuad(center: ReadVec2f, dimensions: ReadVec2f, image: Image): Unit = {
		createQuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.draw()
	}

	def drawQuad(centerX: Float, centerY: Float, width: Float, height: Float, color: ReadVec4f, image: Image) = {
		createQuadBuilder()
			.withPosition(centerX, centerY)
			.withDimensions(width, height)
			.withTexture(image)
			.withColor(color)
			.draw()
	}

	def drawQuad(center: ReadVec2f, dimensions: ReadVec2f, color: ReadVec4f, image: Image) = {
		createQuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.withColor(color)
			.draw()
	}

	def drawQuad(center: ReadVec3f, dimensions: ReadVec2f, color: ReadVec4f, image: Image) = {
		createQuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.withColor(color)
			.draw()
	}

	def drawQuad(centerX: Float, centerY: Float, width: Float, height: Float, rotationDegrees: Float, color: ReadVec4f, image: Image) = {
		createQuadBuilder()
			.withPosition(centerX, centerY)
			.withDimensions(width, height)
			.withTexture(image)
			.withColor(color)
			.withRotation(rotationDegrees)
			.draw()
	}

	def drawQuad(center: ReadVec2f, dimensions: ReadVec2f, rotationDegrees: Float, color: ReadVec4f, image: Image) = {
		createQuadBuilder()
			.withPosition(center)
			.withDimensions(dimensions)
			.withTexture(image)
			.withColor(color)
			.withRotation(rotationDegrees)
			.draw()
	}


}


trait TQuadBuilder {

}

class QuadBuilder(vbo: AVBO, textureBlock: TextureBlock, blankTC: Array[ReadVec2f]) extends TQuadBuilder {
	var forward = Vec3f.UnitX
	var ortho = Vec3f.UnitY
	var dimensions = Vec2f.One
	var dimZ = 1.0f
	var color = Vec4f.One
	var texCoords = blankTC
	var cubeFace = -1
	var textureIndexRotation = 0
	var position = Vec3f.Zero

	def withPosition(x: Float, y: Float, z: Float = 0.0f): this.type = {
		position = Vec3f(x, y, z)
		this
	}

	def withPosition(pos: ReadVec2f): this.type = {
		position = Vec3f(pos.x, pos.y, 0.0f)
		this
	}

	def withPosition(pos: ReadVec3f): this.type = {
		position = pos
		this
	}

	def withForward(vec: ReadVec3f): this.type = {
		forward = vec
		this
	}

	def withOrtho(vec: ReadVec3f): this.type = {
		ortho = vec
		this
	}

	def withRotation(degrees: Float): this.type = {
		forward = Vec3f(cosf(toRad(degrees)), sinf(toRad(degrees)), 0.0f)
		ortho = Vec3f(cosf(toRad(degrees + 90.0f)), sinf(toRad(degrees + 90.0f)), 0.0f)
		this
	}

	def withDimensions(s: Float): this.type = {
		dimensions = Vec2f(s, s)
		this
	}

	def withDimensions(x: Float, y: Float): this.type = {
		dimensions = Vec2f(x, y)
		this
	}

	def withDimensions(x: Float, y: Float, z: Float) = {
		dimensions = Vec2f(x, y)
		dimZ = z
		this
	}

	def withDimensions(d: ReadVec2f): this.type = {
		dimensions = d
		this
	}

	def withDimensions(d: ReadVec3f): this.type = {
		dimensions = d.xy
		dimZ = d.z
		this
	}

	def withColor(r: Float, g: Float, b: Float, a: Float): this.type = {
		color = Vec4f(r, g, b, a)
		this
	}

	def withColor(rgba: ReadVec4f): this.type = {
		color = rgba
		this
	}

	def withTexture(image: Image): this.type = {
		texCoords = textureBlock(image)
		this
	}

	def withTexture(imageStr: String): this.type = {
		texCoords = textureBlock(ResourceManager.image(imageStr))
		this
	}

	def withSubTexture(image: Image, pcntRect: Rectf): this.type = {
		val fullRect = textureBlock.getOrElseUpdateRectFor(image)
		val effRect = Rectf(fullRect.x + fullRect.w * pcntRect.x, fullRect.y + fullRect.h * pcntRect.y,
			fullRect.w * pcntRect.w, fullRect.h * pcntRect.h)
		withTexCoords(effRect)
	}

	def withTexCoords(effRect : Rectf) : this.type = {
		texCoords = Array(effRect.xy, Vec2f(effRect.maxX, effRect.y), Vec2f(effRect.maxX,effRect.maxY), Vec2f(effRect.x,effRect.maxY))
		this
	}

	def withTexCoords(tc : Array[ReadVec2f]) : this.type = {
		texCoords = tc
		this
	}

	def withCubeFace(q: Int): this.type = {
		cubeFace = q
		this
	}

	def withTextureIndexRotation(r : Int) : this.type = {
		textureIndexRotation = r
		this
	}

	def draw(): Unit = {
		val vi = vbo.incrementVertexOffset(4)
		val ii = vbo.incrementIndexOffset(6)

		var i = 0
		while (i < 4) {
			if (cubeFace == -1) {
				val x = position.x + forward.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.x * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val y = position.y + forward.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.y * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				val z = position.z + forward.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).x * dimensions.x) + ortho.z * (Cardinals.centeredCubePoints(Cardinals.Top)(i).y * dimensions.y)
				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + i, x, y, z)
			} else {

				val x = position.x + (Cardinals.centeredCubePoints(cubeFace)(i).x * dimensions.x)
				val y = position.y + (Cardinals.centeredCubePoints(cubeFace)(i).y * dimensions.y)
				val z = position.z + (Cardinals.centeredCubePoints(cubeFace)(i).z * dimZ)
				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + i, x, y, z)
			}
			vbo.setAbf(SimpleAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 255)
			vbo.setA(SimpleAttributeProfile.TexCoordAttribute, vi + i, texCoords((i + textureIndexRotation) % 4))
			i += 1
		}
		vbo.setIQuad(ii, vi)
	}

}