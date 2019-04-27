package arx.graphics.text

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/15
 * Time: 9:05 PM
 */

import arx.Prelude._
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.graphics.Image
import arx.graphics.text.PreRenderedGlyphSource.CharMapping
import arx.resource.ResourceManager

import scalaxy.loops._

class PreRenderedGlyphSource(preRenderConfig : ConfigValue) extends GlyphSource {
	val charMappings = {
		preRenderConfig.chars.fields.map {
			case (char,sml) => char(0) -> CharMapping(Vec2i(sml.imgLocation.v2),Vec2i(sml.imgDims.v2),Vec2i(sml.offsets.v2))
		}.toMap
	}
	val charset = charMappings.keys.toSet
	val lineHeightPixels = preRenderConfig.lineHeight.int
	val baseImage = ResourceManager.image(preRenderConfig.imageName.str)
	val minimumOffset = charMappings.values.imin(_.offsets.y)

	override def canProvideGlyphFor(char: Char): Boolean = {
		charset.contains(char)
	}
	override def glyphFor(char: Char): Image = {
		val mapping = charMappings(char)
		val ry = baseImage.height - 1 - (mapping.pos.y + mapping.dim.y)
		val img = Image.withDimensions(mapping.dim.x,lineHeightPixels)
		for (	x <- mapping.pos.x until mapping.pos.x + mapping.dim.x optimized;
				y <- ry until ry + mapping.dim.y + 1 optimized)
		{
			val offY = mapping.offsets.y
			val targetY = (y - ry) + (lineHeightPixels - (offY - minimumOffset) - mapping.dim.y - 1)
			val sourceY = y

			if (sourceY >= 0 && sourceY < baseImage.height) {
				img(x - mapping.pos.x,targetY) = baseImage(x,sourceY)
			}
		}

		img
//		new SubImageView(baseImage,new Rect[Int](mapping.pos.x,mapping.pos.y,mapping.dim.x,mapping.dim.y))
	}
}

object PreRenderedGlyphSource {
	case class CharMapping (pos : ReadVec2i, dim : ReadVec2i, offsets : ReadVec2i)

	def existsFor (name : String, fontSize : Int) = ResourceManager.hasResourceStream(s"fonts/${name}_${fontSize}_compiled.sml")
	def fromFontName (name : String, fontSize : Int) = {
		val sml = ResourceManager.sml(s"fonts/${name}_${fontSize}_compiled.sml")
		new PreRenderedGlyphSource(sml)
	}
}