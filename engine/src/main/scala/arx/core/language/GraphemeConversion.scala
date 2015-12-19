package arx.core.language

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/13/13
 * Time: 3:13 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._


import arx.application.Noto
import arx.resource.ResourceManager
import java.util

object GraphemeConversion {
	val vowels = Set("a","e","i","o","u")
	val vowelChars = vowels.map(_.apply(0))

	case class GraphemeMapping ( grapheme : String, conditions : List[GraphemeCondition], phoneme : String )

	abstract class GraphemeCondition extends ((String,String,Int) => Boolean) {}

	case class FollowedByCondition(followingString:String,lastInWord:Boolean) extends GraphemeCondition {
		def apply ( string : String, grapheme : String, index : Int) = {
			if ( string.length > index + grapheme.length + followingString.length ) {
				if ( string.substring(index + grapheme.length,index + grapheme.length + followingString.length) == followingString ) {
					! lastInWord || string.lastIndexOf(followingString) == index + grapheme.length
				} else {
					false
				}
			} else { false }
		}
	}
	case class PreceededByCondition(preceedingString:String) extends GraphemeCondition {
		def apply ( string : String, grapheme : String, index : Int) = {
			index >= preceedingString.length &&
			string.substring(index - preceedingString.length,index) == preceedingString
		}
	}
	object PreceededByVowel extends GraphemeCondition {
		def apply ( string : String, grapheme : String, index : Int) = {
			index > 0 && vowels.contains( string.substring(index-1,index).stripAccents )
		}
	}
	object FollowedByVowel extends GraphemeCondition {

		def apply ( string : String, grapheme : String, index : Int) = {
			string.length > index + grapheme.length + 1 && vowels.contains( string.substring(index+grapheme.length,index+grapheme.length+1).stripAccents )
		}
	}
	object Unstressed extends GraphemeCondition {
		def apply ( string : String, grapheme : String, index : Int) = false
	}
	class ClosestVowelGraphemeCondition ( condVowels : Set[Char] ) extends GraphemeCondition {
		def apply ( string : String, grapheme : String, index : Int) : Boolean = {
			val unaccented = string.stripAccents
			var dist = 1
			while ( dist <= index || dist < string.length - index ) {
				if ( condVowels contains unaccented(index+dist) ) { return true }
				else if ( vowelChars contains unaccented(index+dist) ) { return false }

				if ( condVowels contains unaccented(index-dist) ) { return true }
				else if ( vowelChars contains unaccented(index-dist) ) { return false }
				dist += 1
			}
			false
		}
	}
	object Broad extends ClosestVowelGraphemeCondition(Set('a','o','u'))
	object Slender extends ClosestVowelGraphemeCondition(Set('e','i'))

	case class AndCondition(a : GraphemeCondition,b : GraphemeCondition) extends GraphemeCondition {
		def apply ( string : String, grapheme : String, index : Int) = {
			a(string,grapheme,index) && b(string,grapheme,index)
		}
	}


	trait TGraphemeMapping {
		def convertToGraphemes ( phonemeString : String ) : String
		def convertToPhonemes ( graphemeString : String ) : String
	}
	class GraphemeMap extends TGraphemeMapping {
		var mappings = List[GraphemeMapping]()
		var graphemesByPhoneme = Map[String,List[String]]()

		def convertToGraphemes(phonemeString: String) = {
			val output = new StringBuilder
			for ( phoneme <- phonemeString.split("/") ) {
				if ( phoneme.nonEmpty ) {
					val possibles = graphemesByPhoneme(phoneme)
					output.append( randFrom(possibles) )
				}
			}
			output.toString
		}
		def convertToPhonemes(graphemeString: String) = {
			val output = new StringBuilder
			var index = 0
			while ( index < graphemeString.length ) {
				var clusterSize = 3
				var phonemeResult : String = ""
				while ( clusterSize > 0 && index + clusterSize <= graphemeString.length && phonemeResult.isEmpty ) {
					val cluster = graphemeString.substring(index,index + clusterSize)
					mappings.find { mapping =>
						mapping.grapheme == cluster &&
						mapping.conditions.forall ( condition => condition(graphemeString,cluster,index) )
					} match {
						case Some(mapping) =>
							phonemeResult = mapping.phoneme
							index += clusterSize
						case None =>
					}
					clusterSize -= 1
				}

				if ( phonemeResult.isEmpty ) { index += 1 }
				else { output.append("/").append(phonemeResult) }
			}
			output.append("/")
			output.toString()
		}
	}

	def buildGraphemeMap ( mapPath : String ) = {
		val graphemeMap = new GraphemeMap

		val text = ResourceManager.getResourceText(mapPath)
		val lines = text.split("\n").map(_.trim).filterNot( l => l.startsWith("//") || l.isEmpty )
		for ( line <- lines ) {
			val onColon = line.split(":")
			if ( onColon.length == 2 ) {
				val grapheme = onColon(0).trim
				val mappings = onColon(1).split(",")
				for ( mappingString <- mappings ) {
					val sections = mappingString.trim.split("/").map(_.trim)
					if ( sections.length == 2 || sections.length == 3 ) {
						val post = if ( sections.length == 3 ) { sections(2) } else { "" }
						val pre = sections(0)
						val phoneme = sections(1)

						var conditions = List[GraphemeCondition]()
						if ( pre != "" ) {
							if ( pre.endsWith("-") ) {
								conditions ::= PreceededByCondition(pre.substring(0,pre.length-1))
							} else { Noto.warn("Unknown prceeding condition \"" + pre + "\" in mapping string " + mappingString) }
						}
						if ( post != "" ) {
							if ( post.startsWith("-") ) {
								if ( post.endsWith("$") ) {
									conditions ::= FollowedByCondition(post.substring(1,post.length - 1),lastInWord = true)
								} else {
									conditions ::= FollowedByCondition(post.substring(1),lastInWord = false)
								}
							} else if ( post == "u" ) {
								conditions ::= Unstressed
							} else if ( post == "b" ) {
								conditions ::= Broad
							} else if ( post == "s" ) {
								conditions ::= Slender
							} else { Noto.warn("Unknown post condition \"" + post + "\" in mapping string " + mappingString) }
						}

						graphemeMap.mappings ::= GraphemeMapping(grapheme,conditions,phoneme)
					} else { Noto.warn("Mapping string \"" + mappingString + "\" was invalid in string : " + mappingString) }
				}
			} else { Noto.warn("Non-mapping line in grapheme map file") }
		}

		//we want the longest graphemes first, otherwise 't' will always match before 'th' gets a chance,
		//we also want graphemes with the most specific conditions to come before more general ones, for the
		//same reason
		graphemeMap.mappings = graphemeMap.mappings.sortWith( (a : GraphemeMapping,b : GraphemeMapping) => {
			if ( a.grapheme.length > b.grapheme.length ) {
				true
			} else if ( a.grapheme.length < b.grapheme.length ) {
				false
			} else {
				a.conditions.length <= b.conditions.length
			}
		} )
		for ( mapping <- graphemeMap.mappings ) {
			val newList = mapping.grapheme :: graphemeMap.graphemesByPhoneme.getOrElse(mapping.phoneme,Nil)
			graphemeMap.graphemesByPhoneme += (mapping.phoneme -> newList)
		}

		graphemeMap
	}
}
