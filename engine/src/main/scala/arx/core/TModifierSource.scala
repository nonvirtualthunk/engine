package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/10/12
 * Time: 8:00 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

trait TModifierSource {
	var createdModifiers = List[Modifier[_]]()

//	def registerModifier[T](moddable : Moddable[T] ) { createdModifiers ::= moddable }

	def createModifier[T](moddable : Moddable[T],func: (T) => T): FunctionModifier[T] = {
		val modifier = new FunctionModifier[T](moddable,func)
		createdModifiers = modifier :: createdModifiers
		modifier
	}

	def tearDownCreatedModifiers () {
		createdModifiers.foreach ( _ match {
			case fm : FunctionModifier[_] => fm.active = false
			case _ =>
		} )
		createdModifiers = Nil
	}


}