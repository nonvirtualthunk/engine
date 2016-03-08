package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 9:08 AM
 */

import arx.engine.data.MapBackedExternalAuxDataStore
import arx.engine.data.TExternalAuxDataStore
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasExternalAuxData

class MinimalGameEntity(val id : Long) extends TGameEntity with THasExternalAuxData[TGameEntityAuxData] {
	override def externalStore: TExternalAuxDataStore[TGameEntityAuxData] = {
		MinimalGameEntity.auxStore
	}

	override def archetype: Option[GameArchetype] = this.aux[ArchetypeAuxData].archetype
	override def archetype_= (arc : GameArchetype) { this.aux[ArchetypeAuxData].archetype = Some(arc) }
	def name = "MinimalEntity(" + id + ")"
}

class MinimalGameEntityWrapper extends TGameEntity with THasExternalAuxData[TGameEntityAuxData] {
	var id = 0L
	def name = "MinimalEntityWrapper(" + id + ")"



	override def archetype: Option[GameArchetype] = this.aux[ArchetypeAuxData].archetype
	override def archetype_= (arc : GameArchetype) { this.aux[ArchetypeAuxData].archetype = Some(arc) }
	override def externalStore: TExternalAuxDataStore[TGameEntityAuxData] = {
		MinimalGameEntity.auxStore
	}
}

object MinimalGameEntity {
	val auxStore = new MapBackedExternalAuxDataStore[TGameEntityAuxData]

	def apply(id : Long) = new MinimalGameEntity(id)
	def apply() = new MinimalGameEntity(TGameEntity.IdCounter.getAndIncrement)
}

protected[entity] class ArchetypeAuxData extends TGameEntityAuxData {
	var archetype : Option[GameArchetype] = None
}