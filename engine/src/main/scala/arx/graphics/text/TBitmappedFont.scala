package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 12:57 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec.ReadVec2f

trait TBitmappedFont {
	def characterTexCoords(c: Char): Array[ReadVec2f]
	def characterWidth(c : Char): Float
	def characterHeight(c : Char): Float

	def characterWidthPixels(c : Char): Int = ???
	def characterHeightPixels(c : Char): Int = ???

	def bind ( i : Int)

	def maxCharacterDimensions : ReadVec2f
}