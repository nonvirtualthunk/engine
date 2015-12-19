package arx.core.language.util

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/15
 * Time: 9:19 AM
 */

import java.text.Normalizer

import arx.Prelude._
import scalaxy.loops._

object LanguageFunctions {
	val vowelCharBuffer = new StringBuilder
	def isVowel ( char : Char ) = {
		vowelCharBuffer.append(char)
		val res = Normalizer.normalize(vowelCharBuffer,Normalizer.Form.NFKD)
		vowelCharBuffer.clear()
		val normedChar = if ( res.isEmpty ) {
			Character.toLowerCase(char)
		} else {
			Character.toLowerCase(res.charAt(0))
		}

		( normedChar == 'a' || normedChar == 'e' || normedChar == 'i' ||
			normedChar == 'o' || normedChar == 'u' || normedChar == 'y' )
	}

}
