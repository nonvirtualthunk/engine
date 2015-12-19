package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/12
 * Time: 2:30 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

object GeneralityLevel extends Enumeration {
	type GeneralityLevel = Value
	/**
	 * A condition that would only match a single instance in a set
	 */
	val Individual = Value
	/**
	 * A condition that would only match a small sub-set of a set
	 */
	val Specific = Value
	/**
	 * A condition that is limited but might match a significant sub-class of things within a set
	 */
	val Limited = Value
	/**
	 * A condition that matches a fairly wide sub-set of a set, but excludes some
	 */
	val Inclusive = Value
	/**
	 * A condition that only excludes specific sub-sets or edge cases within a set
	 */
	val General = Value
	/**
	 * A condition that matches all elements of a set
	 */
	val Unrestricted = Value
	/**
	 * Fallback value, least restricted, contains everything and more
	 */
	val Fallback = Value
}