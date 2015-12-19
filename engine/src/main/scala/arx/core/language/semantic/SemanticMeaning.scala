package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/22/14
 * Time: 1:28 PM
 */

import arx.Prelude._
import arx.application.Noto
import scalaxy.loops._

class SemanticMeaning(val baseMeaning : SemanticEntryPointer) {
	var connotations : List[DictionaryPointer] = Nil
}
class VerbPhrase(val verb : SemanticEntryPointer, val nObject : SemanticEntryPointer) extends SemanticMeaning(verb) {
}
class DitransitivePhrase(val verb : SemanticEntryPointer, val nObject : SemanticEntryPointer) extends SemanticMeaning(verb) {
}

object SemanticMeaning {
	/** The point at which we can no longer express the meaning as the relationship between other base elements,
	  * for example, we can express "soil" as "earth -growing", i.e. same meaning as earth but with an
	  * additional, specific connotation, but we can't effectively define "earth" in terms of the
	  * other words we have defined. */
	val Base = new SemanticMeaning(EmptySemanticEntry)

	def fromString (str : String) = {
		val tokens = str.split(" ")
		if (tokens.isEmpty) {
			Base
		} else {
			val iter = tokens.iterator

			try {
				val main = iter.next () match {
					case "to" => {
						val verb = iter.next ()
						iter.next () match {
							case "into" | "to" => {
								val nObject = iter.next()
								new DitransitivePhrase(verb,nObject)
							}
							case nObject => {
								new VerbPhrase(verb,nObject)
							}
						}
					}
					case o => new SemanticMeaning(o)
				}
				while (iter.hasNext) {
					val word = iter.next()
					if (word.startsWith("-")) {
						main.connotations ::= word.substring(1)
					}
				}
				main
			} catch {
				case nse: NoSuchElementException => {
					Noto.warn (f"Semantic meaning '$str' had invalid format")
					Base
				}
			}
		}
	}
}
