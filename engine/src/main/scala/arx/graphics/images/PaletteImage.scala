package arx.graphics.images

/**
  * TODO: Add javadoc
  */

import arx.core.traits.TIdentifiable
import arx.core.vec.ReadVec4f
import arx.graphics.Image
import arx.graphics.helpers.Color
import arx.graphics.helpers.HSBA

import scalaxy.loops._

/**
  * A grouping of related colors, a subsection of the palette
  */
case class PaletteGroup(colors: Vector[Int], name: Option[String] = None)
object PaletteGroup {
	def fromColors(colors : Vector[HSBA]) = {
		PaletteGroup(colors.map(hsba => Color.toInt(hsba.toRGBAi)))
	}
}

case class Palette(groups: Vector[PaletteGroup], _identifier: AnyRef) extends TIdentifiable {
	override def identifier: String = _identifier.toString
}

trait PaletteImage {
	def withPalette(newPalette: Palette) : Image
}

class PaletteImageFull(rawImage: Image, palette: Palette) extends PaletteImage {
	var variations = Map[Palette, Image]()

	def withPalette(newPalette: Palette) = {
		val mapping = Map(0 -> 0) ++ palette.groups.zip(newPalette.groups).flatMap { case (a, b) => a.colors.zip(b.colors) }.toMap

		val newImg = Image.withDimensions(rawImage.width, rawImage.height)
		val newData = newImg.data.asIntBuffer()
		val oldData = rawImage.data.asIntBuffer()

		for (i <- 0 until newImg.width * newImg.height optimized) {
			newData.put(i, mapping(oldData.get(i)))
		}

		newImg
	}
}

object PaletteImage {
	protected def colorSort(i : Int) = {
		val tmp = Color.fromInt(i)
		tmp.r + tmp.g + tmp.b + tmp.a
	}

	def fromImage(img: Image, implicitPalette: Boolean): PaletteImage = {
		if (implicitPalette) {
			var colors = Set[Int]()
			val intBuff = img.data.asIntBuffer()
			for (i <- 0 until img.width * img.height optimized) {
				colors += intBuff.get(i)
			}
			colors -= 0 // ignore full-alpha
			//			val hsbaColors = colors.map(Color.fromInt).map(Color.RGBAtoHSBA).toVector
			new PaletteImageFull(img, Palette(Vector(PaletteGroup(colors.toVector.sortBy(i => colorSort(i)))), "Base"))
		} else {
			???
		}
	}
}
