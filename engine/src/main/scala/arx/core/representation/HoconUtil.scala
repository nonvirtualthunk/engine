package arx.core.representation

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/15
 * Time: 9:25 PM
 */

import arx.Prelude._
import arx.core.vec.{ReadVec4i, ReadVec3i, ReadVec2i}
import scalaxy.loops._

object HoconUtil {
	def toHoconString (map : Map[String,Any], tabbing : String = "", output : StringBuilder = new StringBuilder) : String = {
		output.append("{\n")
		for ((k,v) <- map) {
			output.append(tabbing).append("\t").append(k).append(" : ")
			collapseToStr(v,output,tabbing)
			output.append("\n")
		}
		output.append(tabbing).append("}\n").append(tabbing)

		output.toString()
	}

	protected def collapseToStr (v : Any, output : StringBuilder,tabbing: String): Unit = {
		v match {
			case digStr : String if digStr.forall(_.isDigit) => output.append(digStr)
			case quoStr : String if quoStr.startsWith("\"") => output.append(quoStr)
			case str : String => output.append("\"").append(str).append("\"")
			case b : Boolean => output.append(b)
			case i : Int => output.append(i)
			case v2 : ReadVec2i => output.append("[").append(v2.x).append(",").append(v2.y).append("]")
			case v3 : ReadVec3i => output.append("[").append(v3.x).append(",").append(v3.y).append(",").append(v3.z).append("]")
			case v4 : ReadVec4i => output.append("[").append(v4.r).append(",").append(v4.g).append(",").append(v4.b).append(",").append(v4.a).append("]")
			case m : Map[String,Any] => toHoconString(m,tabbing + "\t",output)
			case col : List[_] =>
				output.append("[")
				for (elem <- col) {
					collapseToStr(elem,output,tabbing)
					output.append(",")
				}
				output.append("]")
		}
	}
}
