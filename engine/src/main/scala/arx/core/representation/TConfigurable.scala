package arx.core.representation

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/4/13
 * Time: 8:13 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

trait TConfigurable {
	def setFromSML ( sml : ConfigValue , overwrite : Boolean = false )
}