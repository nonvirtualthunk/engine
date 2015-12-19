package arx.core.parsing

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/11/14
 * Time: 7:34 AM
 */

import arx.Prelude._
import scalaxy.loops._

object ParseUtil {
	/** Parses expressions that take the form : <br />
	  * <code> somePrefix ( innerValue ) </code><br />
	  * <code> somePrefix(innerPrefix(innermost)) </code><br />
	  */
	def parsePrefixAndParens (str : String, recurse : Boolean) : Expression = {
		val innerParenIdx = str.indexOf("(")
		val outerParenIdx = str.lastIndexOf(")")
		if (innerParenIdx == -1 || outerParenIdx == -1) {
			EmptyExpression
		} else {
			val prefix = str.substring(0,innerParenIdx).trim
			val argument = str.substring(innerParenIdx+1,outerParenIdx).trim
			if (recurse) {
				parsePrefixAndParens(argument,recurse) match {
					case EmptyExpression => SimpleExpression(prefix,argument)
					case subExpression : Expression => CompoundExpression(prefix,subExpression)
				}
			} else {
				SimpleExpression(prefix,argument)
			}
		}
	}

	class Expression (label : String, value : Either[Expression,String])
	case class SimpleExpression (l_ : String, v : String) extends Expression (l_,scala.Right(v))
	case class CompoundExpression (l_ : String, subExpression : Expression) extends Expression(l_,scala.Left(subExpression))
	case object EmptyExpression extends Expression ("",scala.Right(""))
}
