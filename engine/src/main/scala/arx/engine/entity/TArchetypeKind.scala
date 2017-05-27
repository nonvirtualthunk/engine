package arx.engine.entity

import arx.core.introspection.TEagerSingleton

/**
 * Typed kind of archetype (avoids stringly typed)
 */
trait TArchetypeKind extends TEagerSingleton {
	def kindStr = this.getClass.getSimpleName.takeWhile(_ != '$').capitalize

	def Sentinel : GameArchetype

	def parentKind : Option[TArchetypeKind] = None
	def isA (kind : TArchetypeKind) : Boolean = kindStr == kind.kindStr || parentKind.exists(k => k.isA(kind))

	override def toString: String = kindStr
}
