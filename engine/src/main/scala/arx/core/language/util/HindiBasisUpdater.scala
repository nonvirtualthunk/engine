package arx.core.language.util

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/17/14
 * Time: 8:44 AM
 */

import java.io.File


import arx.Prelude._
import arx.core.gen.StringMarkov
import arx.core.language.LanguageBasis
import scalaxy.loops._

class HindiBasisUpdater {
	val auxPath = "src/main/auxiliary/language/hindi/"
	val rsrcPath = "src/main/resources/axis/language/hindi/"

	def main (args : Array[String]) {
		val basisFile = new File(rsrcPath + "hindi.basis")
		val basis = readFromFile[LanguageBasis](basisFile)

		val maleNames = new File(rsrcPath + "hindiMaleNames.txt").lines.toSet
		val femaleNames = new File(rsrcPath+ "hindiFemaleNames.txt").lines.toSet

		println("Male names :\n" + maleNames)
		println("\n\nFemale name : \n" + femaleNames)

		val maleChain = new StringMarkov(3)
		val femaleChain = new StringMarkov(3)

		maleChain.trainStrings(maleNames)
		femaleChain.trainStrings(femaleNames)

		maleChain.finishTraining()
		femaleChain.finishTraining()

		basis.maleNameChains = maleChain
		basis.femaleNameChains = femaleChain

		writeToFile(basisFile,basis)
	}
}
