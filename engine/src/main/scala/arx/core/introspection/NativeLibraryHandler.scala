package arx.core.introspection

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/18/15
 * Time: 3:26 PM
 */

import java.io._

import arx.Prelude._
import scala.io.Source
import scalaxy.loops._

object NativeLibraryHandler {
	val outDir = new File("/tmp/libs")
	
	def extract (name : String): Unit = {
		if (!outDir.exists()) { outDir.mkdirs() }

		val outFile = new File(outDir,name)
		if (! outFile.exists) {
			val libstr = getClass.getClassLoader.getResourceAsStream(name)
			if (libstr != null) {
				val outStream = new FileOutputStream(outFile)
				copy(libstr,outStream)
				libstr.close()
				outStream.close()
			}
		}
	}
	def extractVariationsOf (name : String) = {
		extract(s"$name.dylib")
		extract(s"$name.so")

		val winName = name - "lib"
		extract(s"$winName.dll")


	}

	def copy (inStream : InputStream, outStream : OutputStream): Unit = {
		val buffer = Array.ofDim[Byte](1024 * 4) // 4 meg-ish buffer
		var broken = false
		while (! broken) {
			inStream.read(buffer) match {
				case -1 => broken = true
				case n => outStream.write(buffer,0,n)
			}
		}
	}
	
	def extractAll (): Unit = {
//		extractVariationsOf("libgdx-bullet64")
//		extractVariationsOf("libgdx-bullet")
		extractVariationsOf("liblwjgl")
		extractVariationsOf("libglfw")
//		extractVariationsOf("libgdx")
//		extractVariationsOf("libgdx64")

		extract("openal.dylib")
		extract("OpenAL32.dll")
		extract("OpenAL64.dll")
	}

	def load (): Unit = {
		extractAll()

		System.setProperty("java.library.path",outDir.getAbsolutePath)
		System.setProperty("org.lwjgl.librarypath",outDir.getAbsolutePath)
//		val all = outDir.listFiles().toList
//		if (System.getProperty("os.name").toLowerCase().contains("mac")) {
//			val macFiles = all.filter(f => f.getName.endsWith(".dylib") || f.getName.endsWith(".so"))
//			macFiles.foreach(f => {
//				println(s"Loading : ${f.getName}")
//				System.load(f.getAbsolutePath)
//			})
//		}
	}

	def main(args: Array[String]) {

	}
}
