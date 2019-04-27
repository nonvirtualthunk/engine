package arx.graphics.text

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/15
 * Time: 9:03 PM
 */

import arx.graphics.Image

@SerialVersionUID(1L)
trait GlyphSource extends Serializable {
	def canProvideGlyphFor (char : Char) : Boolean
	def glyphFor (char : Char) : Image
	def lineHeightPixels : Int
}
