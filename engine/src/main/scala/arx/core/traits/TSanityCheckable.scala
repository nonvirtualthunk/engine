package arx.core.traits

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 2/7/16
  * Time: 2:31 PM
  */

import arx.application.Noto


trait TSanityCheckable {
	def Assert ( condition : Boolean , message : String ) = TSanityCheckable.Assert(condition,message)
	def sanityChecks : List[TSanityCheckable.Assert] = Nil

	/** Runs through all defined sanity checks on this object and prints out messages for any that failed. Returns
	  * true if the object is sane, false otherwise.
	  */
	def checkSanity (recursive : Boolean = false) : Boolean = {
		val failedChecks = sanityChecks filterNot ( _.condition )
		failedChecks foreach ( a => Noto.warn("Sanity failed (" + this + "): " + a.message) )
		failedChecks.isEmpty
	}
}
object TSanityCheckable {
	case class Assert ( condition : Boolean , message : String )
}