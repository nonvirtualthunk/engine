package arx.core.representation

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 3/5/16
  * Time: 11:27 AM
  */

import arx.Prelude._
import arx.resource.ResourceManager

object ConfigAssistant {
	/**
	  * Extracts from the given resource all packages, extracts from each package the field at
	  * leafFieldName, returns a mapping from individual entry name to individual entry
	  */
	def loadAllConfigsByPackage(baseResource: String, leafFieldName: String): Map[CaseInsensitiveString, ConfigValue] = {
		val topLevelSML = ResourceManager.smlAll(baseResource)
		val packages = topLevelSML.fields.map(tup => new ConfigPackage(tup._1, tup._2.field(leafFieldName).fields))
		packages.flatMap(pack => {
			pack.children.map {
				case (k, v) => new CaseInsensitiveString(k) -> v
			}
		}).toMap
	}
}

class ConfigPackage(val name: String, val children: Map[String, ConfigValue]) {

}