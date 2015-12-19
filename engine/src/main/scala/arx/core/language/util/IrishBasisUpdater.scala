package arx.core.language.util

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/17/14
 * Time: 8:27 AM
 */

import java.io.File

import arx.Prelude._

import arx.core.gen.StringMarkov
import arx.core.language.LanguageBasis
import scalaxy.loops._

object IrishBasisUpdater {
	val auxPath = "src/main/auxiliary/language/irish/"
	val rsrcPath = "src/main/resources/axis/language/irish/"

	def main (args : Array[String]) {
		val basisFile = new File(rsrcPath + "irish.basis")
		val basis = readFromFile[LanguageBasis](basisFile)

		val maleNames = new File(auxPath + "irish_names_masculine.txt").lines
		val femaleNames = new File(auxPath + "irish_names_feminine.txt").lines

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
