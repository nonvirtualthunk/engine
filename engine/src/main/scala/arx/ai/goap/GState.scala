package arx.ai.goap

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.ai.goap.CostType.ImpossibleType
import arx.application.Noto
import arx.core.async.Executor
import arx.core.datastructures.Killable
import arx.core.datastructures.voxelregions.voxelregions.VoxelRegion
import arx.core.traits.ArxEnum
import arx.core.traits.ArxEnumObject
import arx.core.vec.coordinates.VoxelCoord
import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.EntityReference
import arx.engine.entity.GameEntity
import arx.engine.entity.TGameEntity
import arx.engine.requirement.AuxDataDescriptor
import arx.engine.requirement.CoordinateDescriptor
import arx.engine.requirement.Descriptor
import arx.engine.requirement.EntityDescriptor
import arx.engine.requirement.SpecificEntityDescriptor
import arx.engine.world.World

/*
So the question here is, to some extent, how do we decide what is possible, in the sense of, do we specifically target
ok, we need to move here, so make a move action between those two points? Or do we make move actions for all locations
in range? Probably in between, but how, precisely.

{Path 1}
Desired State(Fed)
Eat Food (HasItem(Food) -> Fed)
Pick up Food (NearItem(Food) -> HasItem(Food))
Move to Item (ItemExists(Food) -> NearItem(Food))

{Path 2}
Desired State(Fed)
Eat Food : (HasItem(Food) -> Fed)
Pick up Food : (NearItem(Food) -> HasItem(Food))
Move to Item : (ItemExists(Food) -> NearItem(Food))
Cast Spell[Fireball] : ((KnowsSpell(Fireball),CanCast(Fireball)) -> (DamageInArea(Target), FireInArea(Target)))
	conditional : (ItemExistsInArea(Meat, Target) -> ItemExists(Food))
Place Meat : (HasItem(Meat) -> (NearItem(Meat),ItemExistsInArea(Meat, Target))

{Path 3}


Path 1 and Path 2 can be differentiated by cost, to the point that 2 would not be chosen unless 1 was not a possibility.
Let's give this a try using only meta, then determine the extent to which we need the other approach

It seems like the big question we're coming down to is, to some extent, do we express the states and pre/post conditions
by actually making modifications to a (presumably copied) world, or do we express them purely in meta-language. Do we
make a set of constructs just for expressing pre/post conditions and states. We definitely want some meta in order to
narrow the search space significantly. We don't want to try casting every spell on the off chance that one of them
happens to create a food item.

 */


class StateKind(name: String) extends ArxEnum(name) {}

object StateKind extends ArxEnumObject[StateKind]

abstract class GState {
	def pcntMetBy(world: World): Float

	def equivalentTo(world: World): Traversable[GState] = Nil

	def isEquivalentTo(world: World, state: GState) = this == state || equivalentTo(world).exists(s => s == state)

	def isSubsetOf(world: World, state: GState) = isEquivalentTo(world, state)

	//	def stateKinds : List[StateKind]
	def unary_~ : GState = new GNegatedState(this)
}
class GNegatedState(val baseState : GState) extends GState {
	override def pcntMetBy(world: World): Float = 1.0f - baseState.pcntMetBy(world)

	override def equivalentTo(world: World): Traversable[GState] = Nil

	override def isEquivalentTo(world: World, state: GState): Boolean = state match {
		case ns : GNegatedState => this.baseState.isEquivalentTo(world, ns.baseState)
		case _ => false
	}

	override def isSubsetOf(world: World, state: GState): Boolean = state match {
		case ns : GNegatedState => this.baseState.isSubsetOf(world, ns.baseState)
		case _ => false
	}

	override def unary_~ : GState = baseState
}

abstract class GAction {
	// this can apply it either to a copy, or to the actual, live world. How to differentiate? Cam we make it transparent?
	def applyAction(world: World, isCopy: Boolean): Boolean

	def preconditions: List[GState]

	def postconditions: List[GState]

	/**
	  * Heuristic cost calculator, intended to find the minimum possible cost to execute this action
	  * given what we know about the current states
	  */
	def minimumCost(currentStates: List[GState], action: GAction): Map[CostType, Float]

	def cost(world: World, action: GAction): Map[CostType, Float]
}

abstract class GActionProvider {
	def possibleActions(world: World): PartialFunction[GState, Traversable[GAction]]

	def possibleActions(world: World, state: GState): List[GAction] = {
		val func = possibleActions(world)
		func.applyOrElse(state, (s: GState) => Nil).toList
	}
}


case class GEquivalentAction(toState: GState, equivalentState: GState) extends GAction {
	override def applyAction(world: World, isCopy: Boolean): Boolean = {
		// do nothing, this is just adapting between equivalent states
		true
	}

	override def preconditions: List[GState] = List(equivalentState)

	override def postconditions: List[GState] = List(toState)

	// there is no cost, this is just a redefinition of terms
	override def minimumCost(currentStates: List[GState], action: GAction): Map[CostType, Float] = Map()

	override def cost(world: World, action: GAction): Map[CostType, Float] = Map()

	override def toString: String = s"ToEquivalent($equivalentState -> $toState)"
}

object GEquivalentAction extends GActionProvider {
	override def possibleActions(world: World): PartialFunction[GState, Traversable[GAction]] = {
		case state => state.equivalentTo(world).map(equiv =>
			GEquivalentAction(state, equiv))
	}
}

//case class GBeInStateAction(state: GState) extends GAction {
//	override def applyAction(world: World, isCopy: Boolean): Boolean = true
//	override def preconditions: List[GState] = Nil
//	override def postconditions: List[GState] = List(state)
//	override def minimumCost(currentStates: List[GState], action: GAction): Map[CostType, Float] = Map()
//	override def cost(world: World, action: GAction): Map[CostType, Float] = Map()
//}
//object GBeInStateAction extends GActionProvider {
//	override def possibleActions(world: World): PartialFunction[GState, Traversable[GAction]] = {
//		case state if state.pcntMetBy(world) >= 1.0f => List(GBeInStateAction(state))
//	}
//}


case class Plan(actions: Vector[GAction], pcntBenefit: Float) {
	override def toString = {
		"Plan {\n" +
			"\tActions {" + (if (actions.isEmpty) {
			""
		} else {
			"\n" + actions.map("\t\t" + _.toString).reduce(_ + "\n" + _) + "\n"
		}) + "\t}\n" +
			"\tBenefit: " + pcntBenefit + "\n}"
	}
}

class CostType(name: String) extends ArxEnum(name)

object CostType extends ArxEnumObject[CostType] {
	val Injury = CostType("Injury")
	val Time = CostType("Time")
	val Exertion = CostType("Exertion")
	val Item = CostType("Item")
	/**
	  * Special cost for indicating that something simply cannot be done
	  */
	val ImpossibleType = CostType("Impossible")
}

object Cost {
	val Impossible = Map(ImpossibleType -> 10000000.0f)
}


object GOAPSandbox {

	class PhysicalData extends TGameEntityAuxData {
		var position: VoxelCoord = VoxelCoord.Center
		var size = 1.0f

		override def equals(obj: scala.Any): Boolean = obj match {
			case id: PhysicalData => id.position == position
			case _ => false
		}
	}

	class CharacterData extends TGameEntityAuxData {
		var calories = 100.0f

		override def equals(obj: scala.Any): Boolean = obj match {
			case id: CharacterData => id.calories == calories
			case _ => false
		}
	}

	class FoodData extends TGameEntityAuxData {
		var caloriesProvided = 0.0f
		var cookable = false

		override def equals(obj: scala.Any): Boolean = obj match {
			case fd: FoodData => fd.caloriesProvided == caloriesProvided && fd.cookable == cookable
			case _ => false
		}
	}

	class InventoryData extends TGameEntityAuxData {
		var items = List[TGameEntity]()

		override def equals(obj: scala.Any): Boolean = obj match {
			case id: InventoryData => id.items == items
			case _ => false
		}
	}

	// costs : time, calories(?), injury
	/*
	All of the post-conditions have the possibility of being a cost, you could burn a spear for warmth
	and that might be an expedient solution to a chill, but it would be at the cost of destroying a
	useful item. The trick there is primarily around recognizing something's primary purpose, or I suppose
	recognizing its value. It's tricky though! If you went by, say, monetary value, you could get into
	a situation where someone would nearly starve to death before eating an expensive cake. So really, you
	do care about the primary purpose, specifically you're looking at it as opportunity cost, if you
	use it for X then what activity Y can you not use it for? A cake has no other use (other than sale)
	so it's worth eating, even if it is valuable, though cheaper fulfilling food should go first. The
	spear though has value as a weapon, burning it would produce far less value. But how do you express that?
	It seems somewhat reasonable to have items know their primary purposes, that could be used to amplify
	the presumed cost of using it for something for which it is not intended, while maintaining the
	possibility. The idea of someone burning their spear in desperation to avoid dying of exposure is
	pretty cool.

	One concept we had been thinking about is to keep a general set of goal states that represent an
	individual's ambient concerns, Fed, Healthy, Rested, Safe (not near enemies). These could all be
	analyzed after a plan is hypothetically executed to determine the extent to which other goals have
	been detracted by advancing this other goal. It's a tempting unification of the goal states to
	work towards with the calculation of cost. Going inside to sleep has an opportunity cost against
	being fed, and cooking food has an opportunity cost against eating something simpler that takes
	less time. On the other hand I suspect that it may break down fairly quickly. It's hard to express
	a general interest in time spent, except indirectly as calories expended and sleep needed, though
	perhaps that's sufficient. Further, it's difficult to express group-wide goals in this way, like
	the value of a weapon not being burned. There isn't really a goal state there other than "have things"
	which doesn't really fit. We could certainly do a hybrid, for every goal state outstanding we compute
	the extent to which it has been made less satisfied, then we additionally compute costs from straight
	cost-calculators that could analyze the pre/post world and the plan itself.
	 */

	case class FedState(character: EntityReference) extends GState {
		override def pcntMetBy(world: World): Float = {
			val CD = character.resolve(world).auxData[CharacterData]
			CD.calories / 1000.0f
		}
	}

	case class HasItemState(character: EntityReference, item: EntityDescriptor) extends GState {
		override def pcntMetBy(world: World): Float = {
			character.resolve(world).auxDataOpt[InventoryData] match {
				case Some(idata) =>
					if (idata.items.contains(item)) {
						1.0f
					} else {
						0.0f
					}
				case None => 0.0f
			}
		}

		override def isSubsetOf(world: World, state: GState): Boolean = state match {
			case HasItemState(otherChar, otherItem) if character == otherChar =>
				item.resolve(world).isSubsetOf(otherItem.resolve(world))
			case _ => false
		}
	}


	case object FoodDescriptor extends AuxDataDescriptor[FoodData](f => true) {
		override def toString = "FoodDescriptor"
	}

	case object IngredientDescriptor extends AuxDataDescriptor[FoodData](f => f.cookable) {
		override def toString = "IngredientDescriptor"

		override def isSubsetOf(other: Descriptor): Boolean = other match {
			case FoodDescriptor => true
			case IngredientDescriptor => true
			case _ => false
		}
	}

	case class EatAction(actorRef: EntityReference) extends GAction {
		def itemToEat(world: World) = {
			val actor = actorRef(world)
			actor[InventoryData].items.filter(e => e.hasAuxData[FoodData]) match {
				case Nil => None
				case list => Some(list.maxBy(e => e.aux[FoodData].caloriesProvided))
			}
		}

		override def applyAction(world: World, isCopy: Boolean): Boolean = {
			val actor = actorRef(world)
			itemToEat(world) match {
				case Some(e) =>
					actor.auxData[CharacterData].calories += e[FoodData].caloriesProvided
					actor.auxData[InventoryData].items = actor.auxData[InventoryData].items.without(e)
					world.removeEntity(e)
					true
				case None =>
					false
			}
		}

		override def preconditions: List[GState] = List(HasItemState(actorRef, FoodDescriptor))

		override def postconditions: List[GState] = List(FedState(actorRef))

		override def minimumCost(currentStates: List[GState], action: GAction): Map[CostType, Float] =
			Map(CostType.Time -> 1.0f)

		override def cost(world: World, action: GAction): Map[CostType, Float] =
			itemToEat(world) match {
				case Some(e) => Map(CostType.Time -> e.aux[PhysicalData].size)
				case None => Cost.Impossible
			}
	}

	object EatAction extends GActionProvider {
		override def possibleActions(world: World): PartialFunction[GState, Traversable[GAction]] = {
			case FedState(character) => EatAction(character) :: Nil
		}
	}

	case class NearEntityState(character: EntityReference, item: EntityDescriptor) extends GState {
		val reach = 5.0f

		def closestMatchingObject(world: World, c: EntityReference) = c.in(world).auxDataOpt[PhysicalData] match {
			case Some(pd) =>
				val allObjects = world.auxDataQuery[PhysicalData]
				val matchingObjects = allObjects.filter(o => item.isSatisfiedBy(o))
				if (matchingObjects.nonEmpty) {
					Some(matchingObjects.map(o => o -> distance(pd.position, o[PhysicalData].position)).minBy(_._2))
				} else {
					None
				}
			case None =>
				Noto.error("Character testing for near state, but is non-physical")
				None
		}

		override def pcntMetBy(world: World): Float = {
			closestMatchingObject(world, character) match {
				case Some((_, minDist)) if minDist < reach => 1.0f
				case _ => 0.0f
			}
		}

		override def equivalentTo(world: World): Traversable[GState] = {
			val matchingObjects = item match {
				case SpecificEntityDescriptor(spec) => List(spec)
				case _ =>
					val allObjects = world.auxDataQuery[PhysicalData]
					allObjects.filter(o => item.isSatisfiedBy(o))
			}

			matchingObjects.map(o => InAreaState(character, WithinDistanceFromPoint(o[PhysicalData].position, reach)))
		}
	}

	case class WithinDistanceFromPoint(point: VoxelCoord, maxDist: Float) extends CoordinateDescriptor {
		override def allMatching = VoxelRegion.apply(point, maxDist.toInt)

		override def matchesTyped(entity: VoxelCoord): Boolean = distance(entity, point) < maxDist

		override def boundingRegion: VoxelRegion = VoxelRegion.apply(point, maxDist.ceili)
	}

	case class InAreaState(character: EntityReference, area: CoordinateDescriptor) extends GState {
		override def pcntMetBy(world: World): Float = {
			character.in(world).auxDataOpt[PhysicalData] match {
				case Some(pd) => area.amountSatisfiedBy(pd.position)
				case None => 0.0f
			}
		}
	}

	case class TakeAction(character: EntityReference, item: EntityReference) extends GAction {
		// this can apply it either to a copy, or to the actual, live world. How to differentiate? Cam we make it transparent?
		override def applyAction(world: World, isCopy: Boolean): Boolean = {
			//			NearEntityState(character, item).closestMatchingObject(world, character) match {
			//				case Some((obj, minDist)) if minDist <= 5.0f =>
			//					val actor = character(world)
			//					actor[InventoryData].items ::= obj
			//					true
			//				case _ =>
			//					false
			//			}
			val actor = character.in(world)
			actor[InventoryData].items ::= item.in(world)
			true
		}

		override def preconditions: List[GState] = List(NearEntityState(character, SpecificEntityDescriptor(item)))

		override def postconditions: List[GState] = List(HasItemState(character, SpecificEntityDescriptor(item)))

		override def minimumCost(currentStates: List[GState], action: GAction) = Map(CostType.Time -> 1.0f)

		override def cost(world: World, action: GAction) = Map(CostType.Time -> item.in(world).aux[PhysicalData].size)
	}

	object TakeAction extends GActionProvider {
		override def possibleActions(world: World) = {
			case HasItemState(character, item) => item match {
				case SpecificEntityDescriptor(specificItem) => TakeAction(character, specificItem) :: Nil
				case desc: EntityDescriptor => world.auxDataQuery[PhysicalData]
					.filter(i => desc.matchesEntity(i))
					.map(i => TakeAction(character, i))
			}
		}
	}


	case class MoveAction(character: EntityReference, moveTo: CoordinateDescriptor) extends GAction {
		override def applyAction(world: World, isCopy: Boolean): Boolean = {
			val actor = character(world)
			actor.auxDataOpt[PhysicalData] match {
				case Some(pd) =>
					val possibilities = moveTo.allMatching
					if (possibilities.nonEmpty) {
						pd.position = possibilities.minBy(v => distance(v, pd.position))
						true
					} else {
						false
					}
				case None => false
			}
		}

		override def postconditions: List[GState] = List(InAreaState(character, moveTo))

		override def preconditions: List[GState] = List()

		override def minimumCost(currentStates: List[GState], action: GAction) = {
			currentStates.reverse.collect {
				case InAreaState(ch, area) if ch == character => area
			}.headOption match {
				case Some(mostRecentCoordDesc) =>
					val currentlyOccupied = mostRecentCoordDesc.boundingRegion
					val necessaryToOccupy = moveTo.boundingRegion

					val centerDelta = distance(currentlyOccupied.center, necessaryToOccupy.center)
					val doubleSumOfRadii = currentlyOccupied.boundingDimensions.asVec.lengthSafe +
						necessaryToOccupy.boundingDimensions.asVec.lengthSafe
					val minDelta = (centerDelta - (doubleSumOfRadii * 0.5f)).clampFloor(0.0f)
					Map(CostType.Time -> minDelta)
				case None =>
					// needs to know where the individual started out
					Map(CostType.Time -> 1.0f) // added in, this is not a good esitmate, todo: fix
			}
		}

		override def cost(world: World, action: GAction): Map[CostType, Float] = {
			Map()
		}
	}

	object MoveAction extends GActionProvider {
		override def possibleActions(world: World): PartialFunction[GState, Traversable[GAction]] = {
			case InAreaState(character, area) => MoveAction(character, area) :: Nil
		}
	}

	case class CookAction(character: EntityReference) extends GAction {
		override def applyAction(world: World, isCopy: Boolean): Boolean = {
			val actor = character(world)
			val ID = actor[InventoryData]
			val canCook = ID.items.filter(i => IngredientDescriptor.matchesEntity(i))
			canCook match {
				case Nil => false
				case cookable =>
					val shouldCook = cookable.maxBy(f => f[FoodData].caloriesProvided)
					shouldCook[FoodData].cookable = false
					shouldCook[FoodData].caloriesProvided *= 2.0f
					true
			}
		}

		override def preconditions: List[GState] = List(HasItemState(character, IngredientDescriptor))

		override def postconditions: List[GState] = List(HasItemState(character, FoodDescriptor), ~HasItemState(character, IngredientDescriptor))

		/**
		  * Heuristic cost calculator, intended to find the minimum possible cost to execute this action
		  * given what we know about the current states
		  */
		override def minimumCost(currentStates: List[GState], action: GAction): Map[CostType, Float] = Map()

		override def cost(world: World, action: GAction): Map[CostType, Float] = Map()
	}

	object CookAction extends GActionProvider {
		override def possibleActions(world: World): PartialFunction[GState, Traversable[GAction]] = {
			// TODO: overly specific, we'll need this to the point of being able to support arbitrary recipes
			case HasItemState(char, item) if item == FoodDescriptor => {
				CookAction(char) :: Nil
			}
		}
	}


	def main(args: Array[String]): Unit = {
		val world = new World

		val apple = new GameEntity("apply")
		apple.aux[FoodData].caloriesProvided = 100
		apple.aux[PhysicalData].position = VCR(20, -20, 0)

		val haunch = new GameEntity("haunch")
		haunch.aux[FoodData].caloriesProvided = 200
		haunch.aux[FoodData].cookable = true
		haunch.aux[PhysicalData].position = VCR(20, 20, 0)

		val character = new GameEntity()
		character.aux[CharacterData].calories = 100.0f
		character.aux[PhysicalData].position = VCR(-20, 0, 0)

		world.addEntities(character, apple, haunch)

		Noto.info(s"Apple(${apple.id}), Haunch(${haunch.id}, Character(${character.id})")

		GPlanner.plan(world, FedState(character)).foreach(x => Noto.info("Possibility: " + x))

		Executor.onQuit()
		Killable.kill()
	}
}

//class CharacterFedState(character : TGameEntity) extends GGoalState {
//	override def isMetBy(world: World): Boolean = {
//
//	}
//}