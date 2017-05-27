package arx.engine.requirement

/**
 * TODO: Add javadoc
 */

import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.entity.TGameEntity
import arx.engine.world.World

trait Descriptor extends Requirement {
	amount = 1

	def matches (entity : Any) : Boolean

	override def amountSatisfiedBy(entity: Any): Int = if (matches(entity)) {
		1
	} else {
		0
	}

	def isSubsetOf(other : Descriptor) : Boolean = other == this

	override def resolve(world: World): Descriptor = this
}

object EntityDescriptor {
	def apply (f : (TGameEntity) => Boolean) = {
		new EntityDescriptor {
			override def matchesEntity(entity: TGameEntity): Boolean = f(entity)
		}
	}
	def apply (f : TGameEntity ) : EntityDescriptor = new SpecificEntityDescriptor(f)
}

abstract class TypedDescriptor[T : Manifest] extends Descriptor {
	final def matches(any : Any) = if (manifest[T].runtimeClass.isAssignableFrom(any.getClass)) {
		matches(any.asInstanceOf[T])
	} else {
		false
	}

	def matchesTyped(entity : T) : Boolean
}

object TypedDescriptor {
	def apply[T : Manifest](func : (T) => Boolean) = new TypedDescriptor[T] {
		override def matchesTyped(entity: T): Boolean = func(entity)
	}
}

abstract class CoordinateDescriptor extends TypedDescriptor[VoxelCoord] {
	/**
	  * An optionally implemented method to provide all voxel coordinates that _do_
	  * match the descriptor. If None is returned it can likely be assumed that
	  * calculating the full set is inadvisable
	  */
	def allMatching : Traversable[VoxelCoord]
	def boundingRegion : VoxelRegion
}

trait EntityDescriptor extends Descriptor {
	override def matches(entity: Any): Boolean = entity match {
		case ent : TGameEntity => matchesEntity(ent)
		case _ => false
	}

	def matchesEntity(entity: TGameEntity): Boolean

	def and(other : EntityDescriptor) : EntityDescriptor = new CombinedEntityDescriptor(this, other)
}

class CombinedEntityDescriptor(a : EntityDescriptor, b : EntityDescriptor) extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = {
		a.matchesEntity(entity) && b.matchesEntity(entity)
	}
}