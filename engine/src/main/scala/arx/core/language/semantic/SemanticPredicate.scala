package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/8/14
 * Time: 9:15 AM
 */

class SemanticPredicate {
	/** Does this predicate imply the relationship in both directions? For example, "weapon.associatedWith : war"
	  * implies (at least somewhat) that "war.associatedWith : weapon". */
	var symmetrical = false
	/** Is there a corresponding predicate that is implicitly running the other direction, e.g.
	  * having "leaf.partOf : tree" implies "tree.hasPart : leaf" */
	var bidirectionalWith : Option[String] = None
}
