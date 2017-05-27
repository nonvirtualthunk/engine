package arx.engine.entity

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:16 AM
 */

import java.util.concurrent.atomic.AtomicLong

import arx.Prelude._
import arx.application.Noto
import arx.core.introspection.CopyAssistant
import arx.core.traits.TSentinelable
import arx.engine.data.TCopyOnWriteAuxData
import arx.engine.data.TGameEntityAuxData
import arx.engine.data.THasAuxData
import arx.engine.data.THasInternalAuxData
import arx.engine.data.WrappedWithData
import arx.engine.world.World

import scalaxy.loops._

trait TGameEntity extends THasAuxData[TGameEntityAuxData] with TSentinelable {
	var world : World = World.Sentinel

	def name : String
	def archetype : Option[GameArchetype]
	def archetype_= (arc : GameArchetype)

	def archetypeIsA(kind : TArchetypeKind) = archetype.exists(a => a.kind.isA(kind))

	override def withData[R <: TGameEntityAuxData : Manifest] : WrappedWithData[TGameEntityAuxData,R,TGameEntity] =
		new WrappedWithData[TGameEntityAuxData, R, TGameEntity](this)

	def copyDataFrom[R <: TGameEntityAuxData : Manifest](other : TGameEntity): Unit = {
		if (other.hasAuxData[R]) {
			this.manualAddAuxData(CopyAssistant.copy(other.aux[R]))
		}
	}

	def <<[R <: TGameEntityAuxData](data : R) = {
		this.manualAddAuxData(data)
	}

	def id : Long

	override def equals(obj: scala.Any): Boolean = obj match {
		case te : TGameEntity => te.id == this.id
		case _ => false
	}

	override def hashCode(): Int = id.hashCode()
}

object TGameEntity {
	val IdCounter = new AtomicLong(1L)
}

class EntityReference (val id : Long) {
	def resolve(world : World) = world.resolve(this)
	def apply(world : World) = world.resolve(this)
	def in(world : World) = world.resolve(this)

	override def toString = s"EntityRef($id)"
	override def equals(o : Any) = o match {
		case ref : EntityReference => ref.id == id
		case ent : TGameEntity => ent.id == id
		case _ => false
	}
}
object EntityReference {
	def apply(ent : TGameEntity) = new EntityReference(ent.id)
	implicit def fromEntity (ent : TGameEntity): EntityReference = new EntityReference(ent.id)
}
