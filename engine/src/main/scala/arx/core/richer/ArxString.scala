package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/13/13
 * Time: 3:05 PM
 * Created by nonvirtualthunk
 */

import java.text.Normalizer
import java.util.regex.Pattern

import arx.application.Noto

import scala.language.postfixOps
import scalaxy.loops._

class ArxString(val intern: String) extends AnyVal {
	def -(other: String) : String = {
		var tmp = intern
		while ( tmp.intern.lastIndexOf(other) >= 0 ) {
			val idx = tmp.intern.lastIndexOf(other)
			tmp = intern.substring(0,idx) + intern.substring(idx + other.length)
		}
		tmp
	}

	def =~= (other:String) : Boolean = {
		if (intern != null) {
			intern.equalsIgnoreCase (other)
		} else {
			intern == other
		}
	}

	def toCamelCase : String = {
		val sb = new StringBuilder
		for ( i <- 0 until intern.length optimized) {
			if ( i != 0 && intern(i-1) == ' ' ) {
				sb.append(intern(i).toUpper)
			} else if ( intern(i) != ' ' ) {
				sb.append(intern(i))
			}
		}
		sb.toString()
	}

	def toSnakeCase : String = {
		val sb = new StringBuilder
		for (i <- 0 until intern.length optimized) {
			if (intern(i) != ' ') {
				sb.append(intern(i).toLower)
			} else {
				sb.append('_')
			}
		}
		sb.toString()
	}

	def fromCamelCase : String = {
		var previousWasSpace = false
		val sb = new StringBuilder
		for ( i <- 0 until intern.size ) {
			if ( ! previousWasSpace && i != 0 && intern(i).isUpper ) {
				sb.append(" ")
			}
			sb.append(intern(i))
			if ( intern(i) == ' ' ) { previousWasSpace = true }
			else { previousWasSpace = false }
		}
		sb.toString()
	}
	def capitalizeAll : String = {
		val sb = new StringBuilder
		for ( i <- 0 until intern.size ) {
			if ( i == 0 || intern(i - 1) == ' ' ) {
				sb.append( intern(i).toUpper )
			} else { sb.append( intern(i) ) }
		}
		sb.toString()
	}

	def stripAccents = {
		val normalized = Normalizer.normalize(intern,Normalizer.Form.NFD)
		val pattern = ArxString.accentRemovalPattern
		pattern.matcher(normalized).replaceAll("")
	}

	def stripPunctuation = {
		intern.replaceAll("\\p{Punct}+","")
	}
	def stripWhitespace = {
		intern.replaceAll("\\s+","")
	}

	def dropRightWhile ( condition : (Char) => Boolean ) = {
		intern.reverse.dropWhile( condition ).reverse
	}
	def takeRightWhile ( condition : (Char) => Boolean ) = {
		intern.reverse.takeWhile( condition ).reverse
	}

	def toIntOpt = {
		try {
			Some(intern.toInt)
		} catch {
			case nfe : NumberFormatException => {
				None
			}
		}
	}

	def toFloatOpt = {
		try {
			Some(intern.toFloat)
		} catch {
			case nfe : NumberFormatException => {
				None
			}
		}
	}

	def toDoubleOpt = {
		try {
			Some(intern.toDouble)
		} catch {
			case nfe : NumberFormatException => {
				None
			}
		}
	}

	def toBooleanOpt = {
		try {
			Some(intern.toBoolean)
		} catch {
			case nfe : NumberFormatException => {
				None
			}
		}
	}

	def extract (regex : String) : List[String] = {
		if (!regex.contains("(")) {
			Noto.warn("String.extract(...) called without any capture regions")
		}
		extract(Pattern.compile(regex))
	}
	def extract (pattern : Pattern) : List[String] = {
		val m = pattern.matcher(intern)
		var ret = List[String]()
		while (m.find) {
			ret ::= m.group(1)
		}
		ret
	}

	def padBack (size : Int,withChar : Char) = {
		val diff = size - intern.size
		if (diff <= 0) {
			intern
		} else {
			import scalaxy.loops._
			val pad = new StringBuilder
			for (i <- 0 until diff optimized) {
				pad.append(withChar)
			}
			intern + pad.toString()
		}
	}
	def padFront (size : Int,withChar : Char) = {
		val diff = size - intern.size
		if (diff <= 0) {
			intern
		} else {
			import scalaxy.loops._
			val pad = new StringBuilder
			for (i <- 0 until diff optimized) {
				pad.append(withChar)
			}
			pad.toString() + intern
		}
	}
}

object ArxString {
	val accentRemovalPattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+")
}