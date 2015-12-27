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

	def name = "MinimalEntity(" + id + ")"
}

class MinimalGameEntityWrapper extends TGameEntity with THasExternalAuxData[TGameEntityAuxData] {
	var id = 0L
	def name = "MinimalEntityWrapper(" + id + ")"

	override def externalStore: TExternalAuxDataStore[TGameEntityAuxData] = {
		MinimalGameEntity.auxStore
	}
}

object MinimalGameEntity {
	val auxStore = new MapBackedExternalAuxDataStore[TGameEntityAuxData]

	def apply(id : Long) = new MinimalGameEntity(id)
	def apply() = new MinimalGameEntity(TGameEntity.IdCounter.getAndIncrement)
}