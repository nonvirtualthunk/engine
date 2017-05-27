package arx.core.traits

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/15
 * Time: 10:30 AM
 */

import arx.Prelude._
import arx.engine.entity.GameArchetype

import scalaxy.loops._

trait TIdentifiable {
	def identifier : String

	override def hashCode(): Int = identifier.hashCode
	override def equals(obj: Any): Boolean = obj match {
		case ident: TIdentifiable => ident.identifier == this.identifier
		case _ => false
	}
}
