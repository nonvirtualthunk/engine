package arx.graphics.helpers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/26/13
 * Time: 6:18 PM
 * Created by nonvirtualthunk
 */

import arx.core.introspection.ReflectionAssistant
import arx.core.math.Rectf
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec4f
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.graphics.text.TBitmappedFont
import arx.graphics.traits.TRenderTarget

object TextRendering {
	val whitespace = Set(' ','\t','\n')
	val textLayouter = ReflectionAssistant.provideInstanceOf[TTextLayouter]

	def renderString ( target : TRenderTarget , font : TBitmappedFont , text : String, bottomLeft: ReadVec3f , dimensionLimits : ReadVec2f , fontSize : Float , color : ReadVec4f , spacingMultiple : Float = 1.0f ) {
		val results = textLayouter.layOutText(text, font, fontSize, Rectf(bottomLeft.x,bottomLeft.y,dimensionLimits.x,dimensionLimits.y),spacingMultiple )

		val points = results.points
		val dims = results.dimensions

		val vbo = target.vbo
		val pos = bottomLeft

		var vi = target.incrementVertexOffset(points.size * 4)
		var ii = target.incrementIndexOffset(points.size * 6)

		val strippedText = text.filterNot(whitespace)
		var index = 0

		val baseOffsetX = (if ( false ) { dims.x * 0.5f } else { 0.0f })
		val baseOffsetY = (if ( false ) { dims.y * 0.5f } else { 0.0f })

		for ( point <- points ) {
			val char = strippedText.charAt(index)

			val cw = textLayouter.charWidth(char, font, fontSize)
			val ch = textLayouter.charHeight(char, font, fontSize)
			val texCoords = font.characterTexCoords(char)

			val baseX = point.x - baseOffsetX
			val baseY = point.y - baseOffsetY - 0.1f * fontSize
			val baseZ = pos.z
			if ( vbo.attribProfile == SimpleAttributeProfile ) {
				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + 0, baseX, baseY, baseZ)
				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + 1, baseX + cw, baseY, baseZ)
				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + 2, baseX + cw, baseY + ch, baseZ)
				vbo.setA(SimpleAttributeProfile.VertexAttribute, vi + 3, baseX, baseY + ch, baseZ)

				var i = 0; while (i < 4) {
					vbo.setA(SimpleAttributeProfile.TexCoordAttribute, vi + i, texCoords(i))
					vbo.setAbf(SimpleAttributeProfile.ColorAttribute, vi + i, color.r, color.g, color.b, color.a, 128)
					i += 1
				}
				vbo.setIQuad(ii,vi)
			} else { throw new IllegalArgumentException("Provided vbo does not have an acceptable attribute profile") }

			vi += 4
			ii += 6
			index += 1
		}
	}
}