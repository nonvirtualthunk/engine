package arx.ai.goap

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.introspection.CopyAssistant
import arx.core.introspection.ReflectionAssistant
import arx.engine.world.CopyOnWriteWorld
import arx.engine.world.World

import scala.annotation.tailrec
import scala.collection.mutable

object GPlanner {
	val actionProviders = ReflectionAssistant.instancesOfSubtypesOf[GActionProvider]

	/**
	  * Note: currentStates are listed in the order in which they occurred, latest in time, latest in
	  * the list
	  */
	case class Node(targetStates: Set[GState], currentStates: List[GState], parent: Option[Node], action : Option[GAction]) {
		def fullPlan : List[GAction] = action.toList ::: parent.map(p => p.fullPlan).getOrElse(Nil)
	}

	def plan(world: World, endState: GState): List[Plan] = {
		val root = Node(Set(endState), Nil, None, None)
		val queue = new mutable.Queue[Node]()
		queue.enqueue(root)

		// in some circumstances, you need to re-plan, the primary example we have of this is movement. If your goal is
		// to cut down the tree you're standing by your plan becomes [go to axe, pick axe, chop down tree], but of course
		// the chop down tree preconditions are no longer met. At runtime the need to replan could be detected fairly
		// easily, but that would still fail to consider the cost correctly without work.
		// Represent it instead as [AlreadyMetByWorld(Near(tree)), GoTo(axe), Take(axe), Chop(tree)]
		// We could detect that the postcondition of GoTo(axe) would invalidate the Near(tree) precondition, then we
		// could re-plan the AlreadyMetByWorld(Near(tree)) subtree, then insert that in. That would presumably
		// need to be done with every parent, but would, presumably only apply to AlreadyMetByWorld goals?
		// Ah, there is an issue we haven't thought of as well, something as simple as "pick up three sticks", you need
		// to be able to mark each stick as in-use by one of your other plan pieces
		// Ok, marking is useful, though we'll need something somewhat unified in order to handle the plans of other
		// entities
		// To return to invalidation, it is unfortunate, as hitting anything that invalidates an early assumption
		// probably requires re-planning the entire thing, which potentially gets you into a weird loop
		// Tentatively thinking it through, you could use marking to avoid many conflicts like the three sticks problem
		// If you simply don't shortcut to an AlreadyMet for position stuff, then you bypass that. Where you _do_ need
		// AlreadyMet is for one-way or complex goals, say, knowing a spell. Learning it would require planning out
		// a whole tree which is entirely unnecessary if you already know it. For another example, the three sticks
		// problem, if you have three sticks in your inventory, it would be pretty dumb to spend the energy to acquire
		// them fresh from somewhere else. Ok, interesting one. Limited inventory space. You need three rods to build
		// this, uh, tripod. You are carrying 1 lead rod, takes up all your weight capacity. You need to drop that
		// rod and pick up three featherwood rods. How do you know to do that in this infrastructure? Taking a rod
		// could of course require a HasInventorySpace state, that's one that could be like "oh that's fine, we have space"
		// already, but then that changes as other things are picked up.
		// Can this then be solved with a variant of marking?
		// [Note: it appears that part of what we are doing here is called Least Commitment planning, and may potentially
		//  be addressed by inequality constraints discussed on page 394 of http://aima.cs.berkeley.edu/2nd-ed/newchap11.pdf]
		//

		var ret = List[Plan]()
		while (queue.nonEmpty) {
			val node = queue.dequeue()

			if (node.targetStates.isEmpty) {
				// we have found a path, for the moment, just consider that good enough
				val actionPath = node.fullPlan.toVector
				val benPcnt = computeBenefit(world, actionPath, endState)
				ret ::= Plan(actionPath, benPcnt)
			} else {
				// look at each remaining target state
				for (targetState <- node.targetStates) {
					for (provider <- actionProviders) {
						for (action <- provider.possibleActions(world,targetState)) {
							// add all of the results of completing this action to the set of current states
							val newCurrentStates = action.postconditions ::: node.currentStates
							// add all the preconditions of the action as targets
							var newTargets = node.targetStates ++ action.preconditions
							if (!action.isInstanceOf[GEquivalentAction]) {
								// then remove all targets that are satisfied by (isSubsetOf) the postconditions we will have achieved
								for (post <- action.postconditions) {
									newTargets = newTargets.filterNot(t => post.isSubsetOf(world, t))
								}
							} else {
								newTargets = newTargets -- action.postconditions
							}

							val newNode = Node(newTargets, newCurrentStates, Some(node), Some(action))
							queue.enqueue(newNode)
						}
					}
				}
			}
		}

		ret
	}

	def computeBenefit (world : World, actions : Vector[GAction], endState: GState) = {
		val basePcnt = endState.pcntMetBy(world)
		val deepCopy = CopyAssistant.copy(world)
		val hypothetical = new CopyOnWriteWorld(world)
		actions.foreach(a => a.applyAction(hypothetical, isCopy = true))

		if (!World.deepEquals(world, deepCopy)) {
			Noto.error("Pre/post world states are not equivalent")
		}

		endState.pcntMetBy(hypothetical) - basePcnt
	}

//	def plan(world: World, state: GGoalState): Option[List[Plan]] = {
//		if (state.pcntMetBy(world) >= 1.0f) {
//			Some(Nil) // no plans are necessary
//		} else {
//			// find all the possible actions that could move us to the currently examining goal state
//			val possibleActions = actionProviders.flatMap(p => p.possibleActions(world).lift(state)).flatten
//
//			// take each of those actions
//			val fullPlans = possibleActions.flatMap(a => {
//				// expand out each list of preconditions and attempt to plan those
//				val plansByPreconOpt = a.preconditions.map(p => plan(world, p))
//				// if all of the preconditions can be satisfied
//				if (plansByPreconOpt.forall(planOpt => planOpt.isDefined)) {
//					val plansByPrecon = plansByPreconOpt.map(t => t._1 -> t._2.get)
//
//					// we know they're all possible, so flatten it down to one list of possibilities per precon
//					val preconPlans = preconPlansOpt.flatten
//					// return the
//					Some(preconPlans.flatten.map(p => Plan(p.actions :+ a)))
//				} else {
//					None
//				}
//			})
//
//			if (fullPlans.nonEmpty) {
//				Some(fullPlans.flatten.toList)
//			} else {
//				None
//			}
//		}
//	}

//
//	@tailrec
//	def allVariations(plansByState: List[List[Plan]]): List[Plan] = {
//		plansByState match {
//			case Nil => Nil
//			case only :: Nil => only
//			case head :: tail =>
//				// decide to branch out the head, so take every tail and cross it with the head
//				val withHeadChosen = tail.map(tailPlans => head.cross(tailPlans, _ concat _))
//				allVariations(withHeadChosen)
//		}
//	}
}
