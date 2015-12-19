package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/17/14
 * Time: 8:29 AM
 */

import java.io.File

import arx.Prelude._
import scalaxy.loops._

final class ArxFile (val file : File) extends AnyVal {
	def lines = {
		val src = io.Source.fromFile(file)
		val lines = src.getLines().toList
		src.close()
		lines
	}
}
object ArxFile {
	implicit def enrich (file : File) = new ArxFile(file)
}