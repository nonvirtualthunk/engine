package arx.graphics.text

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/15
 * Time: 9:16 PM
 */

import arx.Prelude
import arx.core.representation.HoconUtil
import arx.core.vec.Vec2i
import arx.resource.ResourceManager

import scala.collection.immutable.ListMap
import scala.collection.immutable.TreeMap
import scala.xml.XML

object SparrowConverter {
	case class Font(name : String, sizes : List[Int])
	
	val fonts = Font("pf_ronda_seven",List(8)) :: Font("modern_antiqua",List(20)) :: Nil

	def main(args: Array[String]) {
		for (font <- fonts ; size <- font.sizes) {
			val inputPath = s"fonts/${font.name}_${size}.fnt"
			val doc = XML.load(ResourceManager.getResourceStream(inputPath))
	
			var output = ListMap[String,Any]()

			output += "imageName" -> s"fonts/${font.name}_${size}_compiled.png"
			output += "lineHeight" -> (doc \\ "common" \@ "lineHeight")

			var charMap = TreeMap[String,Any]()
			for (charDesc <- doc \\ "char") {
				var charObj = Map[String,Any]()
				val letter = (charDesc \@ "letter") match {
					case "space" => " "
					case "\"" => "\\\""
					case "\\" => "\\\\"
					case i => i
				}
	
				val imgLocation = Vec2i((charDesc \@ "x").toInt, (charDesc \@ "y").toInt)
				val imgDims = Vec2i((charDesc \@ "width").toInt, (charDesc \@ "height").toInt)
				val offsets = Vec2i((charDesc \@ "xoffset").toInt, (charDesc \@ "yoffset").toInt)
				charObj += "xadvance" -> (charDesc \@ "xadvance").toInt
	
				charObj += "imgLocation" -> imgLocation
				charObj += "imgDims" -> imgDims
				charObj += "offsets" -> offsets
	
				charMap += ("\"" + letter + "\"") -> charObj
			}
			output += "chars" -> charMap

			val res = HoconUtil.toHoconString(output)

			val outputPath = s"src/main/resources/fonts/${font.name}_${size}_compiled.sml"
			Prelude.writeTextToFile(outputPath,res)
	//		println(commonTag)
		}
	}
}
