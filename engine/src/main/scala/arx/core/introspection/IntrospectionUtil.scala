package arx.core.introspection

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/13/14
 * Time: 7:30 PM
 */

import java.io.Externalizable
import java.io.File
import java.lang.reflect.Modifier

import arx.Prelude._
import arx.application.Noto

object IntrospectionUtil {

	def classForNameOpt (clazzName : String) = {
		try {
			Some(Class.forName(clazzName,false,this.getClass.getClassLoader))
		} catch {
			case cnf : ClassNotFoundException => None
		}
	}

	def specializedFormsOf (clazz : Class[_]) : List[Class[_]] = {
		val possibleSpecializations = List("B","S","I","L","F","D")
		var ret : List[Class[_]] = Nil
		for (spec <- possibleSpecializations) {
			IntrospectionUtil.classForNameOpt(clazz.getCanonicalName + "$mc" + spec + "$sp") match {
				case Some(c) => ret ::= c
				case None =>
			}
		}
		ret
	}
}
