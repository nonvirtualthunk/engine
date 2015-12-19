package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/15
 * Time: 9:17 AM
 */

import arx.Prelude._
import arx.core.representation.ConfigValue
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject

class Sex(name:String) extends ArxEnum(name) {
	var pronoun = ""
	var reflexive = ""
	var possessive = ""
	var accusative = ""
	var sibling = ""
	var parent = ""
	var child = ""
	var spouse = ""
	var nuptial = ""
	var siblingInLaw = ""
	var childInLaw = ""


	def writingTokens = Map(
		"pronoun" -> pronoun,
		"reflexive" -> reflexive,
		"possessive" -> possessive,
		"accusative" -> accusative,
		"sibling" -> sibling,
		"parent" -> parent,
		"child" -> child,
		"spouse" -> spouse,
		"nuptial" -> nuptial,
		"siblingInLaw" -> siblingInLaw,
		"childInLaw" -> childInLaw
	)
}
object Sex extends ArxEnumObject[Sex] {
	val Male = Sex("Male")
	val Female = Sex("Female")
	val Neuter = Sex("Neuter")

	Male.pronoun = "he"
	Male.reflexive = "himself"
	Male.possessive = "his"
	Male.accusative = "him"
	Male.parent = "father"
	Male.sibling = "brother"
	Male.child = "son"
	Male.spouse = "husband"
	Male.nuptial = "groom"
	Male.siblingInLaw = "uncle"
	Male.childInLaw = "nephew"

	Female.pronoun = "she"
	Female.reflexive = "herself"
	Female.possessive = "her"
	Female.accusative = "her"
	Female.parent = "mother"
	Female.sibling = "sister"
	Female.child = "daughter"
	Female.spouse = "wife"
	Female.nuptial = "bride"
	Female.siblingInLaw = "aunt"
	Female.childInLaw = "niece"

	Neuter.pronoun = "it"
	Neuter.reflexive = "itself"
	Neuter.possessive = "its"
	Neuter.accusative = "it"
	Neuter.parent = "parent"
	Neuter.sibling = "sibling"
	Neuter.child = "child"
	Neuter.spouse = "spouse"
	Neuter.nuptial = "wedded"
	Neuter.siblingInLaw = "inlaw"
	Neuter.childInLaw = "cousin"
}