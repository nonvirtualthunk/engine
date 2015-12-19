package arx.core.representation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/6/15
 * Time: 9:20 AM
 */

import arx.Prelude._
import scalaxy.loops._

object ConfigUtil {
	def extractSingularOrPlural ( sml : ConfigValue , singField : String , pluralField : String ) : Traversable[ConfigValue] = {
		if ( sml.hasField(singField) ) { List(sml.field(singField)) }
		else if ( sml.hasField(pluralField) ) { sml.field(pluralField).arr }
		else { Nil }
	}

	def extractStrings (sml : ConfigValue, singField : String, pluralField : String) = {
		extractSingularOrPlural(sml,singField,pluralField).toList.map(_.str)
	}
}
