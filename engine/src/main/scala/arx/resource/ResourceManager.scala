package arx.resource

import java.awt.Font
import java.awt.font.TextAttribute
import java.io._
import java.text.AttributedCharacterIterator
import java.util
import java.util.{Properties, UUID}

import arx.Prelude
import arx.application.Noto
import arx.core.language.LanguageBasis
import arx.core.representation.Hocon.RichConfigRoot
import arx.core.representation._
import arx.graphics.Image
import arx.graphics.TextureBlock
import arx.graphics.shader.ArxShader
import arx.graphics.shader.Shader
import arx.graphics.shader.TUniformProvider
import arx.graphics.text._
import arx.serialization.DecompressibleInputStream
import com.typesafe.config.Config
import overlock.atomicmap.AtomicMap

import scala.collection.mutable.HashMap
import scala.io.Source
import scala.xml.Elem
import scala.xml.XML

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/4/11
 * Time: 11:16 AM
 * To change this template use File | Settings | File Templates.
 */

object ResourceManager {
	def subdirsOf (f : File) : List[File] = {
		f.isDirectory match {
			case true =>
				f.listFiles() match {
					case null => Nil
					case arr => arr.toList
				}
			case false => Nil
		}
	}
	def subdirsOfName ( f : File , name : String , depth : Int ) : List[File] = {
		if ( depth <= 0 ) { Nil }
		else {
			(f.getName match {
				case n : String if ( n.equals(name) ) => List(f)
				case _ => Nil
			}) ::: (subdirsOf(f) flatMap { child => subdirsOfName(child,name,depth - 1) })
		}
	}


	val useLocalResources = System.getProperty("useLocalResources") match {
		case "true" => true
		case _ => false
	}
	Noto.info("Using local resources : "+ useLocalResources)
	val extraLocalResourceDirs = System.getProperty("localResourceDirectory","") match {
		case "" => Nil
		case path => List( new File(path) )
	}
	val localResourceDirs = new File("./src/main/resources") :: extraLocalResourceDirs//subdirsOfName( new File(".") , "resources" , 3 )
	val extensionDirs = List[File]()
	val saveDirectory = new File("./save/")
	if ( ! saveDirectory.exists() ) { saveDirectory.mkdirs() }
	val cacheDirectory = new File(saveDirectory,"caches")
	if ( ! cacheDirectory.exists() ) { cacheDirectory.mkdirs() }
	val internalGuid = UUID.randomUUID()

	class State {
		val images = overlock.atomicmap.AtomicMap.atomicNBHM[String, Image]
		val cacheImages = overlock.atomicmap.AtomicMap.atomicNBHM[String, Image]
		val fonts = overlock.atomicmap.AtomicMap.atomicNBHM[(String,Int,UUID), TBitmappedFont]
		val shaders = overlock.atomicmap.AtomicMap.atomicNBHM[String, Shader]
		val shadersWithProvider = AtomicMap.atomicNBHM[(String,Class[_ <: TUniformProvider]), Shader]
		val xmlFiles = overlock.atomicmap.AtomicMap.atomicNBHM[String, List[Elem]]
		val properties = overlock.atomicmap.AtomicMap.atomicNBHM[String, List[Properties]]
		val languageBasis = overlock.atomicmap.AtomicMap.atomicNBHM[String, LanguageBasis]
		val sml = overlock.atomicmap.AtomicMap.atomicNBHM[String, ConfigValue]
		val loadedSMLReplacements = overlock.atomicmap.AtomicMap.atomicNBHM[String, Boolean]
		val smlReplacements = overlock.atomicmap.AtomicMap.atomicNBHM[String, Any]
		val cacheObjects = overlock.atomicmap.AtomicMap.atomicNBHM[String, AnyRef]
	}

	/**  Removes from the current state all images that have been modified
		* since they were loaded */
	def refreshImages () {
		if (!useLocalResources) {
			Noto.warn("Attempting to reload images, but useLocalResources is not enabled")
		}
		var toRemove = List[String]()
		for ( (key,image) <- state.images ) {
			image.resourcePath match {
				case Some(path) =>
					image.lastModified match {
						case Some(lastModified) =>
							val imgFile = ResourceManager.file(path)
							if ( imgFile.lastModified() > lastModified ) {
								toRemove ::= key
							}
						case None =>
					}
				case None =>
			}
		}

		toRemove.foreach(state.images.remove)
	}

	var state = new State

	def reset() {
		state = new State
	}

	def file ( path : String ) = {
		new File(getResourceFilePath(path))
	}

	def languageBasis ( basePath_pre : String , isBinary : Boolean ) : LanguageBasis = {
		if ( isBinary ) {
			val is = getResourceStream(basePath_pre + ".basis")
			val ois = new DecompressibleInputStream(is)
			val ret = ois.readObject.asInstanceOf[LanguageBasis]
			ois.close()
			is.close()
			ret
		} else {
			loadFromCache( basePath_pre + ".basis" , 0L, {
				val identifier = basePath_pre.substring(basePath_pre.lastIndexOf("/")+1)
				LanguageBasis.buildFromRaw(identifier)
			})
		}
	}

	def xml ( basePath_pre: String ) : List[Elem] = {
		val identifier = pathToIdentifier(basePath_pre)
		state.xmlFiles.getOrElseUpdate( identifier , {
			var ret : List[Elem] = Nil
			for ( stream <- getResourceStreams(basePath_pre) ) {
				ret :+= XML.load(stream)
				stream.close()
			}
			ret
		})
	}

	/**
	 * Ensures that the substitutions located at the given path are loaded in, and will be used
	 * for any subsequent sml parsing
	 */
	@Deprecated
	def loadSMLSubstitutions ( basePath_pre : String ) {
//		val identifier = pathToIdentifier(basePath_pre)
//		state.smlReplacements.getOrElseUpdate( identifier , {
////			for ( path <- getResourceStream(basePath_pre) ) {
////				val source = Source.fromFile(new File(path))
//			val source = Source.fromInputStream(getResourceStream(basePath_pre))
//				val res = Hocon.parse( source.iter, state.smlReplacements ).obj
//			if ( res.isSentinel ) { Noto.warn(s"Trouble parsing sml : $identifier") }
//
//				for ( (key,value) <- res.substitutions.obj.fields ) {
//					state.smlReplacements(key) = value.intern
//				}
//
//				source.close()
////			}
//			true
//		})
	}

	def sml ( basePath_pre : String ) : ConfigValue = {
		smlAll(basePath_pre)
	}

	def smlAll ( basePath_pre : String ) : ConfigValue = {
		val identifier = pathToIdentifier(basePath_pre)
		state.sml.getOrElseUpdate( identifier , {
			var ret : List[Config] = Nil
			for ( stream <- getResourceStreams(basePath_pre) ) {
				val source = Source.fromInputStream(stream)
				val reader = source.bufferedReader()
				try {
					ret :+= Hocon.parseReaderRaw(reader)
				} catch {
					case e : Exception => Noto.warn(s"Exception '${e.getMessage}' encountered while parsing sml '$identifier'")
				}

				reader.close()
				source.close()
			}
			ret match {
				case Nil => ConfigValue.Sentinel
				case head :: tail => {
					var combinedSML = head
					for (value <- tail) {
						combinedSML = combinedSML.withFallback(value)
					}
					RichConfigRoot(combinedSML.root())
				}
			}
		})
	}

	def properties ( basePath_pre: String ) : List[Properties] = {
		state.properties.getOrElseUpdate( basePath_pre , {
			var ret : List[Properties] = Nil
			for ( stream <- getResourceStreams(basePath_pre) ) {
				val p = new util.Properties
				p.load(stream)
				stream.close()
				ret :+= p
			}
			ret
		})
	}


	def shader(basePath_pre: String): Shader = {
		val path = pathToIdentifier(basePath_pre)
		state.shaders.getOrElseUpdate(path, {
			val vertexStream = getResourceStream(path + ".vertex")
			val fragmentStream = getResourceStream(path + ".fragment")

			if ( vertexStream != null && fragmentStream != null ){
				val s = new Shader()
				s.loadFromStreams(vertexStream,fragmentStream)
				s
			} else {
				Noto.severeError("Attempted to load non-existent shader : " + basePath_pre)
				new Shader()
			}
		})
	}

	def reloadShaders() = {
		val newShaderMap = new HashMap[String, Shader]
		for ( (key,shader) <- state.shaders ) {
			val vertexStream = getResourceStream(key + ".vertex")
			val fragmentStream = getResourceStream(key + ".fragment")

			val s = new Shader
			s.loadFromStreams(vertexStream,fragmentStream)
			newShaderMap(key) = s
		}
		for ( (k,v) <- newShaderMap ) {
			state.shaders(k) = v
		}
		ArxShader.allShaders.foreach ( shader => {
			shader.dirty = true
			shader.uniforms.foreach( _.valueInShader = None )
		} )
	}


	def pathToIdentifier(path:String):String = {
		sanitize(path)
	}
	def getNormalizedPath(path: String): String = {
		getResourceFilePath(sanitize(path))
	}

	val defaultImage : Image = state.images.getOrElseUpdate("default/defaultium.png", {
		val stream = getResourceStream("default/defaultium.png")
		val res = if (stream != null) {
			Image.loadFromStream(stream,closeOnFinish = true)
		}
		else {
			Noto.warn("Failed to load default image")
			Image.withDimensions(16,16)
		}
		res.sentinel = true
		res
	})
	defaultImage.sentinel = true

	def blankImage = image("default/blank.png")

	def exists (baseResourcePath : String) : Boolean = {
		getResourceFilePath(baseResourcePath) match {
			case null => false
			case "" => false
			case _ => true
		}
	}


	def imageOpt(baseResourcePath : String) : Option[Image] = {
		val img = getImage(baseResourcePath, warnOnFailure = false)
		if (img eq defaultImage) {
			None
		} else {
			Some(img)
		}
	}
	def imageExists(baseResourcePath: String): Boolean = {
//		val resourcePath = sanitize(baseResourcePath)
//		if ( state.images.contains(resourcePath) ) { ! (state.images(resourcePath) eq defaultImage) }
//		else {
//			val path = getResourceFilePath(resourcePath)
//			new File(path).exists()
//		}
		! (getImage(baseResourcePath,warnOnFailure = false) eq defaultImage)
	}
	def getImage(baseResourcePath: String,warnOnFailure:Boolean = true): Image = {
		val resourcePath = sanitize(baseResourcePath)
		state.images.getOrElseUpdate(resourcePath, {
			val stream = getResourceStream(resourcePath)
			if (stream != null) {
				val img = Image.loadFromStream(stream,closeOnFinish = true)
				img.lastModified = Some(System.currentTimeMillis())
//				timeAndPrint("png colorize") {PNGTransparentColorizer.colorizeTransparency(img)}
				img.resourcePath = Some(resourcePath)
				img
			}
			else {
				if ( warnOnFailure ) { Noto.warn("Failed to load image : " + baseResourcePath + ", no file exists at : " + baseResourcePath) }
				defaultImage
			}
		})
	}
	def image (baseResourcePath:String,warnOnFailure:Boolean = true): Image = getImage(baseResourcePath,warnOnFailure)

	def retrieveFromCache[T <: AnyRef](baseResourcePath:String,ifMoreRecentThan:Long,ifNotPresent: => T) : T = {
		val resourcePath = sanitize(baseResourcePath)
		state.cacheObjects.getOrElseUpdate(resourcePath,loadFromCache(resourcePath,ifMoreRecentThan,ifNotPresent)).asInstanceOf[T]
	}
	private def loadFromCache[T <: AnyRef]( resourcePath:String,ifMoreRecentThan:Long,ifNotPresent: => T) = {
		val file = new File(cacheDirectory,resourcePath)
		if ( file.exists && file.lastModified > ifMoreRecentThan ) {
			val stream = new FileInputStream(file)
			val ostream = new ObjectInputStream(stream)
			val ret = ostream.readObject.asInstanceOf[T]
			ostream.close()
			stream.close()
			ret
		} else {
			if ( ! file.getParentFile.exists ) { file.getParentFile.mkdirs() }
			val ret : T = ifNotPresent

			Prelude.writeToFile(file,ret)

			ret
		}
	}

	def imageCache(baseResourcePath:String,ifMoreRecentThan:Long,ifNotPresent: => Image) = {
		val resourcePath = sanitize(baseResourcePath)
		state.cacheImages.getOrElseUpdate(resourcePath,loadFromImageCache(resourcePath,ifMoreRecentThan,ifNotPresent))
	}

	private def loadFromImageCache( resourcePath:String,ifMoreRecentThan:Long,ifNotPresent: => Image) = {
		val file = new File(cacheDirectory,resourcePath)
		if ( file.exists && file.lastModified > ifMoreRecentThan ) {
			val stream = new FileInputStream(file)
			Image.loadFromStream(stream,closeOnFinish = true)
		} else {
			if ( ! file.getParentFile.exists ) { file.getParentFile.mkdirs() }
			val ret : Image = ifNotPresent

			Image.save(ret,file)

			ret
		}
	}


	def allImagesUnder (baseResourcePath:String) : List[Image] = {
		if ( ! useLocalResources ) { Noto.warn("Cannot do a recursive resource match in non-local resource mode") ; Nil }
		else {
			val rfile = file(baseResourcePath)
			val cleanResourcePath = if ( baseResourcePath.endsWith("/") ) { baseResourcePath } else { baseResourcePath + "/" }
			rfile.listFiles() match {
				case null => Nil
				case files => files.toList.filter( _.getName.endsWith("png") ).map( f => ResourceManager.image(cleanResourcePath + f.getName ) )
			}
		}
	}

	def allFilesOfTypeUnder (baseResourcePath:String, fileType:String) : List[File] = {
		if ( ! useLocalResources ) { Noto.warn("Cannot do a recursive resource match in non-local resource mode") ; Nil }
		else {
			val rfile = file(baseResourcePath)
			val cleanResourcePath = if ( baseResourcePath.endsWith("/") ) { baseResourcePath } else { baseResourcePath + "/" }
			rfile.listFiles() match {
				case null => Nil
				case files => files.toList.filter( _.getName.endsWith(fileType) )
			}
		}
	}

	@deprecated
	def getImageAbsolute( absolutePath : String , warnOnFailure:Boolean = true ) : Image = {
		state.images.find { case (key,value) => value.resourcePath match { case None => false ; case Some(fp) => fp == absolutePath } } match {
			case Some((key,img)) => img
			case None =>
				val idx = absolutePath.indexOf("resources/") + "resources/".length
				getImage(absolutePath.splitAt(idx)._2)
		}
	}

	def font(fontName: String, backingTextureBlock: TextureBlock = null): TBitmappedFont = {
		state.fonts.synchronized {
			val fontStyle = Font.PLAIN
			val key = (fontName,fontStyle,Option(backingTextureBlock).map(b => b.guid).getOrElse(internalGuid))
			val ret = state.fonts.getOrElseUpdate(
				key, {
				println("Updating : " + fontName + " s " + fontStyle)
				try {
					val ttfStream = getResourceStream("fonts/" + fontName + ".ttf")

					var basePointSize = 16.0f
					var pixelFont = false
					if ( ResourceManager.hasResourceStream("fonts/" + fontName + ".sml") ) {
						val sml = Hocon.parseResource("fonts/" + fontName + ".sml")
						basePointSize = sml.basePointSize.floatOrElse(basePointSize)
						pixelFont = sml.pixelFont.boolOrElse(pixelFont)
					}


					val attributes = new util.HashMap[AttributedCharacterIterator.Attribute, Any]()
					attributes.put(TextAttribute.KERNING, TextAttribute.KERNING_ON)
//					attributes.put(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
					val font = if (ttfStream != null) {
						Font.createFont(Font.TRUETYPE_FONT,ttfStream).deriveFont(Font.PLAIN,basePointSize).deriveFont(attributes)
					} else {
						new Font( fontName , fontStyle , basePointSize.toInt ).deriveFont(attributes)
					}

					var additionalSrcList : List[GlyphSource] = Nil
					if (PreRenderedGlyphSource.existsFor(fontName,basePointSize.toInt)) {
						additionalSrcList ::= PreRenderedGlyphSource.fromFontName(fontName,basePointSize.toInt)
					}
					new AdaptiveBitmappedFont(font, additionalSrcList, backingTextureBlock, pixelFont)
				} catch{
					case e : Exception => {
						Noto.warn("Exception encountered while attempting to create font, falling back on bitmapped SansSerif")
						Noto.warn("\texception was : " + e)
						e.printStackTrace()

						val font = new Font("SansSerif",Font.PLAIN, 32)
						new AdaptiveBitmappedFont(font, Nil, backingTextureBlock)
					}
				}
			})
			ret
		}
	}

	def sanitize(str: String) = if (str.startsWith("/")) {
		throw new IllegalStateException("bad path! Was : " + str)
	} else {
		str
	}

	def getSaveFilePath(resourcePath: String) : String = {
		if ( ! useLocalResources ) {
			"./save/" + resourcePath
		} else {
			"./save/" + resourcePath
		}
	}
	def getSaveFile(identifier:String) : File = {
		new File(saveDirectory,identifier)
	}

	def hasResourceStream(resourcePath: String) : Boolean = {
		if ( ! useLocalResources ) {
			this.getClass.getClassLoader.getResourceAsStream(resourcePath) != null
		} else {
			hasResourceStreamLocal(resourcePath) || this.getClass.getClassLoader.getResourceAsStream(resourcePath) != null
		}
	}
	def getResourceStream(resourcePath: String, localFirst : Boolean = useLocalResources) : InputStream = {
		if ( ! localFirst ) {
			val stream = this.getClass.getClassLoader.getResourceAsStream(resourcePath)
			if (stream != null) {
				stream
			}
			else {
				//throw new IllegalStateException("Couldn't find " + resourcePath)
				//If it isn't part of the jar, try looking at a local resource path, if any
				getResourceStreamLocal(resourcePath)
			}
		} else {
			if (hasResourceStreamLocal(resourcePath)) {
				getResourceStreamLocal(resourcePath)
			} else {
				getResourceStream(resourcePath, false)
			}
		}
	}



	protected def hasResourceStreamLocal( resourcePath : String ) : Boolean = {
		localResourceDirs exists { dir =>
			new File( dir , resourcePath ).exists
		}
	}
	protected def getResourceStreamLocal( resourcePath : String ) : InputStream = {
		localResourceDirs find { dir =>
			new File( dir , resourcePath ).exists
		} match {
			case Some(dir) => try {
				new FileInputStream(new File(dir, resourcePath))
			} catch {
				case fnf : FileNotFoundException => {
					Noto.warn("Some sort of timing witchery encountered while trying to get a file input stream")
					null
				}
			}
			case None => null
		}
	}

	def getResourceLines(resourcePath : String) : List[String] = {
		val stream = getResourceStream(resourcePath)
		if ( stream != null ) {
			val src = Source.fromInputStream(stream)
			val ret = src.getLines().toList
			src.close()
			stream.close()
			ret
		} else {
			Noto.warn(f"Null stream for resource path : $resourcePath")
			Nil
		}
	}
	def getResourceText(resourcePath : String) : String = {
		val lines = getResourceLines(resourcePath)
		if ( lines.nonEmpty ) {
			lines.reduceLeft(_ + "\n" + _)
		} else { "" }
	}

	def readObjectFromResource[T](resourcePath: String) : Option[T] = {
		val stream = getResourceStream(resourcePath)
		if ( stream != null ) {
			val objStream = new DecompressibleInputStream(stream)
			val ret = objStream.readObject().asInstanceOf[T]
			objStream.close()
			stream.close()
			Some(ret)
		} else { None }
	}

	def getResourceFilePath(resourcePath: String): String = {
		//		"./resources" + resourcePath;
		if ( ! useLocalResources ) {
			val url = this.getClass.getClassLoader.getResource(resourcePath)
			if (url != null) {
				url.getPath
			}
			else {
				//throw new IllegalStateException("Couldn't find " + resourcePath)
				getResourceFilePathLocal(resourcePath)
			}
		} else {
			getResourceFilePathLocal(resourcePath)
		}
	}
	protected def getResourceFilePathLocal ( resourcePath : String ) : String = {
		localResourceDirs find { dir =>
			new File( dir , resourcePath ).exists
		} match {
			case Some(dir) => new File(dir, resourcePath).getAbsolutePath
			case None =>
				""
		}
	}

	def getResourceStreams (resourcePath: String): List[InputStream] = {
		var ret = List[InputStream]()

		for ( dir <- extensionDirs ; file = new File(dir,resourcePath) ; if ( file.exists ) ) {
			ret ::= new FileInputStream(file)
		}

		if ( ! useLocalResources ) {
			val stream = this.getClass.getClassLoader.getResourceAsStream(resourcePath)
			if (stream != null) {
				ret ::= stream
			}
			else {
				ret :::= getResourceFileStreamsLocal(resourcePath)
			}
		} else {
			val streams = getResourceFileStreamsLocal(resourcePath)
			if (streams.isEmpty) {
				ret :::= (getClass.getClassLoader.getResourceAsStream(resourcePath) match {
					case null => Nil
					case o => List(o)
				})
			} else {
				ret :::= streams
			}

		}
		ret
	}
	protected def getResourceFileStreamsLocal(resourcePath:String):List[InputStream] = {
		var ret = List[InputStream]()
		for ( dir <- localResourceDirs ; file = new File(dir,resourcePath) ; if ( file.exists ) ) {
			ret :+= new FileInputStream(file)
		}
		ret
	}

	def getResourceFilePaths(resourcePath: String): List[String] = {
		var ret : List[String] = Nil

		for ( dir <- extensionDirs ; file = new File(dir,resourcePath) ; if ( file.exists ) ) {
			ret :+= file.getAbsolutePath
		}
		if ( ! useLocalResources ) {
			val url = this.getClass.getClassLoader.getResource(resourcePath)
			if (url != null) {
				ret :+= url.getPath
			}
			else {
				//throw new IllegalStateException("Couldn't find " + resourcePath)
				ret :::= getResourceFilePathsLocal(resourcePath)
			}
		} else {
			ret :::= getResourceFilePathsLocal(resourcePath)
		}
		ret
	}
	protected def getResourceFilePathsLocal(resourcePath:String):List[String] = {
		var ret : List[String] = Nil
		for ( dir <- localResourceDirs ; file = new File(dir,resourcePath) ; if ( file.exists ) ) {
			ret :+= file.getAbsolutePath
		}
		ret
	}
}