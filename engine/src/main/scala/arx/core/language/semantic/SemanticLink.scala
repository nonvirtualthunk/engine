package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/29/14
 * Time: 8:41 AM
 */

case class SemanticLink (from : SemanticEntryPointer, kind : String, to : SemanticEntryPointer) {

}

case class SemanticQuality (from : SemanticEntryPointer, kind : String, quality : String)

object SemanticLink {
	val Connotation = "hasConnotation"
	val Positivity = Connotation
	val Specificity = "hasSpecificity"
	val Association = "associatedWith"
	val Synonym = "synonymOf"
	val Antonym = "antonymOf"


}


object SemanticQuality {
	val Property = "hasProperty"
}