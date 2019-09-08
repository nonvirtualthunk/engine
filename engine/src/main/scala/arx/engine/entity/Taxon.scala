package arx.engine.entity

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/16/18
  * Time: 8:26 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._

class Taxon(val name : String, val parents : List[Taxon]) {

	def isA(other : Taxon) : Boolean = {
		if (other == this) {
			true
		} else {
			parents.exists(t => t.isA(other))
		}
	}

	override def equals(other : Any) : Boolean = {
		other match {
			case t : Taxon => t.name == this.name
			case _ => false
		}
	}
}
object Taxon {
	def apply(name : String, parents : Taxon*) : Taxon = {
		new Taxon(name, parents.toList)
	}

	def apply(name : String, parents : List[Taxon]) : Taxon = {
		new Taxon(name, parents)
	}

}

class Identity(val taxons : List[Taxon]) {
	def this(taxons_ : Taxon*) {
		this(taxons_.toList)
	}
	def isA(x : Taxon) = taxons.exists(t => t.isA(x))
}
object Identity {
	def apply(taxons_ : Taxon*) : Identity = new Identity(taxons_.toList)
	def apply(taxons_ : List[Taxon]) : Identity = new Identity(taxons_)

	implicit def fromTaxon(taxon : Taxon) : Identity = new Identity(taxon)
}

trait HasIdentity {
	val identity : Identity

	def isA(x : Taxon) = identity.isA(x)
}