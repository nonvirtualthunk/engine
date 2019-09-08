package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 12:57 PM
 * Created by nonvirtualthunk
 */

import java.awt.Font

import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i

trait TBitmappedFont {
	def font : Font

	def characterTexCoords(c: Char): Array[ReadVec2f]
	def characterWidthProportional(c : Char): Float
	def characterHeightProportional(c : Char): Float

	def characterWidthPixels(c : Char): Int
	def characterHeightPixels(c : Char): Int

	def characterAdvancePixels(c : Char) : Int

	def bind ( i : Int)

	def fontMetrics : FontMetrics

	def pixelFont : Boolean

	def deriveWithPtSize(fontSize : Int) : TBitmappedFont
}

case class FontMetrics(maxAscent : Int, maxDescent : Int, lineHeight : Int, pointSize : Int, spaceSize : Int)