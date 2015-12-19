package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/29/14
 * Time: 8:39 AM
 */

import arx.Prelude._
import scalaxy.loops._

object SemanticEntryPointer {
	implicit def toSemanticEntry (dl : SemanticEntryPointer) = dl.resolveEntry
	protected object sentinel extends DictionaryPointer("") {
		override def resolveEntry = EmptySemanticEntry
	}
	val Sentinel = sentinel

	implicit def toLink (str : String) = {
		new DictionaryPointer(str)
	}
}

trait SemanticEntryPointer {
	def coreWord : String
	def resolveEntry : SemanticEntry

	override def equals (other : Any) = {
		other match {
			case sep : SemanticEntryPointer => sep.coreWord == this.coreWord
			case str : String => coreWord =~= str
			case _ => false
		}
	}
	override def hashCode = coreWord.hashCode
}