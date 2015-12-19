package arx.core.language

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/5/14
 * Time: 11:07 AM
 */

import java.util.Locale

import arx.Prelude._
import arx.core.gen.{StringMarkov}
import arx.resource.ResourceManager
import scalaxy.loops._

class LanguageBasis(var identifier : String) {
	def this() { this("ShouldHaveDeserialized") }
	var maleNameChains : StringMarkov = StringMarkov.Sentinel
	var femaleNameChains : StringMarkov = StringMarkov.Sentinel
	var languageChains : StringMarkov = StringMarkov.Sentinel
	var graphemeMap : Option[GraphemeConversion.TGraphemeMapping] = None
	var locale : Locale = Locale.getDefault
}


object LanguageBasis {
	def buildFromRaw(identifier : String) : LanguageBasis = {
		val basis = new LanguageBasis(identifier)
		basis.maleNameChains = new StringMarkov(3,0.15f)
		basis.femaleNameChains = new StringMarkov(3,0.15f)
		basis.languageChains = new StringMarkov(3,0.15f)

		if (ResourceManager.exists(f"axis/language/$identifier/allNames.txt")) {
			basis.maleNameChains.trainStrings (
				ResourceManager.getResourceLines (f"axis/language/$identifier/allNames.txt")
			)
			basis.femaleNameChains = basis.maleNameChains
		} else {
			basis.maleNameChains.trainStrings (
				ResourceManager.getResourceLines (f"axis/language/$identifier/maleNames.txt")
			)

			basis.femaleNameChains.trainStrings (
				ResourceManager.getResourceLines (f"axis/language/$identifier/femaleNames.txt")
			)
		}

		basis.languageChains.trainStrings(
			ResourceManager.getResourceLines(f"axis/language/$identifier/wordlist.txt")
		)

		basis.maleNameChains.finishTraining()
		basis.femaleNameChains.finishTraining()
		basis.languageChains.finishTraining()

		basis
	}
}