package arx.engine.world

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 3:24 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.query.ContinuousQuery
import arx.core.query.TContinuousQuerySource
import arx.core.synchronization.ReadWriteLock
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.engine.data._
import arx.engine.entity.GameEntity
import arx.engine.entity.MinimalGameEntity
import arx.engine.entity.MinimalGameEntityWrapper
import arx.engine.entity.TGameEntity
import com.carrotsearch.hppc.LongObjectOpenHashMap
import com.carrotsearch.hppc.LongOpenHashSet
import scalaxy.loops._

class World extends TContinuousQuerySource with THasInternalAuxData[TWorldAuxData] with TSentinelable {
	protected[engine] var entityAuxDataQueries = Map[Class[_],ContinuousQuery[TGameEntity]]()
	protected[engine] var entityQueries = List[ContinuousQuery[TGameEntity]]()

	protected[engine] val minimalEntities = new LongOpenHashSet()
	protected[engine] val fullEntities = new LongObjectOpenHashMap[GameEntity]()

	@transient
	protected[engine] var _entityLock = new ReadWriteLock

	protected[engine] val timeData : TimeData = this.auxData[TimeData]
	def time = timeData.time

	def addEntities(es : TGameEntity*): Unit = {
		es.foreach(addEntity)
	}
	def addEntity(e : TGameEntity): Unit = {
		entityLock.writeLock {
			e match {
				case full : GameEntity =>
					fullEntities.put(full.id,full)
				case minimal : MinimalGameEntity =>
					minimalEntities.add(minimal.id)
				case wrapper : MinimalGameEntityWrapper =>
					Noto.warn("Adding a wrapper entity to the world, you should avoid doing that")
					minimalEntities.add(wrapper.id)
			}
			e.world = this
			// The lock is reentrant, so this shouldn't deadlock, probably
			entityQueries.foreach(_.add(e))
		}
	}

	def removeEntity(e : TGameEntity): Unit = {
		entityLock.writeLock {
			e match {
				case full : GameEntity => fullEntities.remove(full.id)
				case minimal : MinimalGameEntity => minimalEntities.remove(minimal.id)
				case wrapper : MinimalGameEntityWrapper =>
					Noto.warn("Removing a wrapper entity to the world, you should avoid doing that")
					minimalEntities.remove(wrapper.id)
			}
			e.world = World.Sentinel
			// The lock is reentrant, so this shouldn't deadlock, probably
			entityQueries.foreach(_.remove(e))
		}
	}

	def auxDataQuery[T <: TGameEntityAuxData : Manifest] : ContinuousQuery[TGameEntity] = {
		entityLock.writeLock {
			entityAuxDataQueries.get(manifest[T].runtimeClass) match {
				case Some(q) => q
				case None =>
					val q = createEntityQuery { case e : TGameEntity if e.hasAuxData[T] => e }
					entityAuxDataQueries += manifest[T].runtimeClass -> q
					q
			}
		}
	}

	protected[engine] def auxDataAddedToEntity ( entity : TGameEntity , auxData : TGameEntityAuxData ) {
		entityAuxDataQueries.get(auxData.getClass) match {
			case Some(query) => query.add(entity)
			case None =>
		}
	}
	protected[engine] def auxDataRemovedFromEntity ( entity : TGameEntity , auxData : TGameEntityAuxData ) {
		entityAuxDataQueries.get(auxData.getClass) match {
			case Some(query) => query.remove(entity)
			case None =>
		}
	}

	def createEntityQuery ( matchFunction : PartialFunction[Any,TGameEntity]) : ContinuousQuery[TGameEntity] = {
		entityLock.readLock {
			val r = new ContinuousQuery[TGameEntity](matchFunction)

			val entIter = fullEntities.iterator()
			while (entIter.hasNext) {
				val cursor = entIter.next()
				if (matchFunction.isDefinedAt(cursor.value)) {
					r.add(cursor.value)
				}
			}

			val placeholder = new MinimalGameEntityWrapper
			val minIter = minimalEntities.iterator()
			while (minIter.hasNext) {
				val cursor = minIter.next()
				// Use the placeholder to determine if it's actually needed, only instantiate for real if it is
				placeholder.id = cursor.value
				if (matchFunction.isDefinedAt(placeholder)) {
					r.add(MinimalGameEntity(cursor.value))
				}
			}

			registerQuery(r)
			r
		}
	}

	def registerQuery(query: ContinuousQuery[_]) {
		query.source pmatch {
			case Some(src) =>
				Noto.warn(s"Query already had source before registration, detaching old src $src")
				src.unregisterQuery(query)
		}
		entityQueries ::= query.asInstanceOf[ContinuousQuery[TGameEntity]]
		query.source = Some(this)
	}

	def unregisterQuery(query: ContinuousQuery[_]) {
		if ( entityAuxDataQueries.values.exists(_ == query) ) {
			Noto.error("Attempting to unregister aux data query, since these are shared, we can't really do that")
		} else {
			entityQueries = entityQueries without query.asInstanceOf[ContinuousQuery[TGameEntity]]
			query.source = None
		}
	}

	// Transient initialization
	protected[engine] def entityLock = { if (_entityLock == null) { _entityLock = new ReadWriteLock } ; _entityLock }

	override def toString() : String = "World"
}

object World {
	val Sentinel : World = new World with TSentinel
}