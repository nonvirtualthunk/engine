package arx.core.language

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/13/13
 * Time: 3:14 PM
 * Created by nonvirtualthunk
 */


import arx.Prelude._
import arx.core.gen.StringMarkov
import arx.core.language.semantic.Sex
import arx.core.language.util.LanguageFunctions
import arx.core.traits.TSentinel

import scala.collection.mutable

object LanguageData {
	@SerialVersionUID(1L)
	class Suffix ( var string : String , var gender : String , var forNameType : NameType ) {
		def this () { this("","",NameType.Unknown) }

	} ; object Suffix { def apply(string:String,gender:String, forNameType : NameType ) = new Suffix(string,gender,forNameType) }
	@SerialVersionUID(1L)
	class Prefix ( var string : String , var gender : String , var forNameType : NameType ) {
		def this () { this("","",NameType.Unknown) }
	}; object Prefix { def apply(string:String,gender:String,forNameType :NameType) = new Prefix(string,gender,forNameType) }


	@SerialVersionUID(1L)
	class NameType(val description : String) extends Serializable {
		override def toString = description
		override def equals(other:Any) = other match {
			case nt : NameType =>
				(nt eq this) ||
				nt.description == this.description
			case _ => false
		}
		override def hashCode = description.hashCode
	}
	object NameType {
		val Personal = new NameType("personal name")
		val Middle = new NameType("middle name")
		val Family = new NameType("family name")
		val Patronymic = new NameType("patronymic")
		val Place = new NameType("place name")
		val Unknown = new NameType("unknown name type")
	}

	@SerialVersionUID(1L)
	class WordPosition(val description : String) extends Serializable {
		override def toString = description
		override def equals(other:Any) = other match {
			case nt : WordPosition =>
				(nt eq this) ||
				nt.description == this.description
			case _ => false
		}
		override def hashCode = description.hashCode
	}
	object WordPosition {
		val Prefix = new WordPosition("prefix")
		val Suffix = new WordPosition("suffix")
		val Interix = new WordPosition("interix")
		val Unknown = new WordPosition("unknown")

		val allPositions = Map[String,WordPosition]()
		def fromString ( str : String ) = {
			str match {
				case Prefix.description => Prefix
				case Suffix.description => Suffix
				case Interix.description => Interix
				case _ => Unknown
			}
		}
	}

	case class NameRecord ( name: String , nameType : NameType ) {
		override def toString = {
			"NameRecord ( name : " + name + ", type : " + nameType + " )"
		}
	}

	@SerialVersionUID(1L)
	class NameSet ( var suffices : List[Suffix] , var prefixes : List[Prefix] , var records : List[NameRecord] ) {
		def this () { this( Nil,Nil,Nil ) }
		override def toString = {
			"NameSet {\n" +
			"\tSuffices : " + suffices + "\n" +
			"\tPrefixes : " + prefixes + "\n" +
			"\tName Records {\n" +
			records.map("\t\t" + _).reduceLeft(_ + "\n" + _) + "\n" +
			"\t}\n" +
			"}"
		}
	}
	object NameSet {
		def apply ( suffices : List[Suffix] , prefixes : List[Prefix] , records : List[NameRecord] ) = new NameSet(suffices,prefixes,records)
	}


	case class ToponymRoot ( string : String , position : WordPosition )

	@SerialVersionUID(1L)
	class Toponymy extends Serializable {
		var rootsByAttribute = Map[String,ToponymRoot]()

		def getOrElseUpdateAttribute ( attribute : String , updateStatement : => ToponymRoot ) =
			if ( rootsByAttribute.contains(attribute) ) {
				rootsByAttribute(attribute)
			} else {
				val newOne = updateStatement
				rootsByAttribute += attribute -> newOne
				newOne
			}

		override def toString = {
			var out = ""
			out += "Toponymy {\n"
			out += "\tRoots {\n"
			for ( (attribute,root) <- rootsByAttribute ) {
				out += "\t\t" + root.string + " : " + attribute + " : " + root.position + "\n"
			}
			out += "\t}\n"
			out += "}\n"

			out
		}
		def fromString(str : String) = {
			rootsByAttribute = Map()

			val start = str.indexOf("Toponymy")
			val body = str.indexOf("{",start)
			val rootsInd = str.indexOf("Roots",body)
			val rootsBody = str.indexOf("{",rootsInd)
			val rootsEnd = str.indexOf("}",rootsBody)

			val rootsText = str.substring(rootsBody+1,rootsEnd+1)
			val rootLines = rootsText.split("\n")
			for ( rootLine <- rootLines if rootLine.nonEmpty ) {
				val sections = rootLine.split(":")
				if ( sections.size == 3 ) {
					rootsByAttribute += sections(1) -> ToponymRoot(sections(0),WordPosition.fromString(sections(2)))
				}
			}
		}
	}

	object Toponymy {
		import PlaceAttributes._
		val rawPlaceDescriptionToAttributeMap = Map(
		"grove" -> Forest,
		"field" -> Plain,
		"settlement" -> City,
		"forest" -> Forest,
		"woodland" -> Forest,
		"rock" -> Rocky,
		"peak" -> Mountain,
		"homestead" -> City,
		"high" -> High,
		"low" -> Low,
		"pass" -> Pass,
		"small" -> Small,
		"large" -> Large,
		"river" -> River,
		"ford" -> Ford,
		"fort" -> Fort,
		"level land" -> Plain,
		"plain" -> Plain,
		"meadow" -> Plain,
		"hollow" -> Valley,
		"hill" -> Hill,
		"nook" -> Valley,
		"church" -> Shrine,
		"bridge" -> Ford,
		"ridge" -> Ridge,
		"stronghold" -> Fort,
		"island" -> Island,
		"heather" -> Plain,
		"valley" -> Valley,
		"hillside" -> Hill,
		"lake" -> Lake,
		"peatland" -> Marsh,
		"summit" -> Mountain,
		"mill" -> City,
		"big" -> Large,
		"new" -> New,
		"old" -> Old,
		"hole" -> Valley,
		"landing place" -> Ford,
		"marsh" -> Marsh,
		"hawthorn" -> Forest,
		"refuge" -> Fort,
		"beach" -> Ford,
		"burial mound" -> Burial,
		"hillock" -> Hill
		)
	}

	object PlaceAttributes {
		val Rocky = "rocky"
		val River = "river"
		val Ford = "ford"
		val Fort = "fort"
		val Forest = "forest"
		val Ridge = "ridge"
		val Mountain = "mountain"
		val Pass = "pass"
		val Plain = "plain"
		val Old = "old"
		val New = "new"
		val Hill = "hill"
		val Burial = "burial"
		val Valley = "valley"
		val City = "city"
		val High = "high"
		val Low = "low"
		val Small = "small"
		val Large = "large"
		val Shrine = "shrine"
		val Island = "island"
		val Lake = "lake"
		val Marsh = "marsh"
	}

	case class Name ( personalName : String, middleName : Option[String], surname : Option[String], epithet : Option[String] ) {
		override def toString = {
			var ret = personalName
			if ( middleName.nonEmpty ) { ret += " " + middleName.get }
			if ( surname.nonEmpty ) { ret += " " + surname.get }
			if ( epithet.nonEmpty ) { ret += " " + epithet.get }
			ret
		}
	}

	trait TGraphemeGenerator {
		def generate : String
	}

	object SentinelGraphemeGenerator extends TGraphemeGenerator {
		def generate = "sentinel"
	}

	class Language {
		var toponymy : Toponymy = new Toponymy
		var graphemeMap : Option[GraphemeConversion.TGraphemeMapping] = None

		var maleNameChains : StringMarkov = StringMarkov.Sentinel
		var femaleNameChains : StringMarkov = StringMarkov.Sentinel
		var languageChains : StringMarkov = StringMarkov.Sentinel

		/** Persistent store of generated words, indexed by their english meaning */
		var generatedWords = new mutable.HashMap[String,String]()
		var generatedPlaceNames = Set[String]()
		var generatedPersonNames = Set[String]()

		def getOrCreateWord ( meaning : String , possessive : Boolean , minLength : Int, maxLength : Int ) : String = {
			generatedWords.getOrElseUpdate(meaning,createWord(possessive,minLength,maxLength))
		}

		def getOrCreatePrefix ( meaning : String, minLength : Int, maxLength : Int ) : String = {
			generatedWords.getOrElseUpdate(meaning,createPrefix(minLength,maxLength))
		}

		/**
		 * Create a new word from this language's chains with the given properties,
		 * the language will attempt to generate a word fitting those criteria, but
		 * is not guaranteed to be able to
		 * @param possessive whether or not this word is a possessive
		 * @param minLength minimum length of the desired word
		 * @param maxLength maximum length of the desired word
		 * @return a new word generated with the given criteria, or an unrestricted new word if none could be found
		 */
		def createWord ( possessive : Boolean , minLength : Int, maxLength : Int ) : String = {
			var tries = 10
			while ( tries >= 0 ) {
				val word = languageChains.generate(3) : String
				if ( word.length >= minLength && word.length <= maxLength ) {
					return word
				}
				tries -= 1
			}
			languageChains.generate(3)
		}

		def prefixWordCondition ( minLength : Int, maxLength : Int )( chain : List[Char] ) = {
			chain.length >= minLength && chain.length <= maxLength && LanguageFunctions.isVowel(chain.last)
		}
		def createPrefix ( minLength : Int , maxLength : Int ) : String = {
			var tries = 10
			while ( tries >= 0 ) {
				val word = languageChains.generate(Nil,prefixWordCondition(minLength,maxLength),1000,3) : String
				if ( word.length >= minLength && word.length <= maxLength ) {
					return word
				}
				tries -= 1
			}
			languageChains.generate(3)
		}

		def generateName ( sex : Sex, minLength : Int , softMaxLength : Int, absoluteMaxLength : Int , prefix : String = "" ) : String = {
			val nameChains = sex match {
				case Sex.Male => maleNameChains
				case Sex.Female => femaleNameChains
				case Sex.Neuter => randFrom(List(maleNameChains,femaleNameChains))
			}

			var tries = 10
			while ( tries >= 0 ) {
				val word = nameChains.generate(prefix.toList,nameChains.passthroughCondition,softMaxLength,3) : String
				if ( tries == 0 || (word.length >= minLength && word.length <= absoluteMaxLength) ) {
					return word
				}
				tries -= 1
			}

			nameChains.generate(3) // this should never be hit
		}

		/**
		 * Generate a new place name based on this language's toponymy, will not produce duplicate names (within reason).
		 * @param placeAttributes attributes of the place to have a name generated, use constants from the
		 *                        <code>PlaceAttributes</code> class
		 * @return a place name
		 */
		def generatePlaceName (placeAttributes : List[String]) : String = {
			val roots = (PlaceAttributes.Island :: placeAttributes).map( a => toponymy.getOrElseUpdateAttribute(a,new ToponymRoot(getOrCreateWord(a,possessive = false,2,5),WordPosition.Prefix)) )
			val chosenRoot = randFrom( roots)
			val name = if ( chosenRoot.position == WordPosition.Prefix ) {
				chosenRoot.string + generateName(Sex.Neuter,2,8,12,chosenRoot.string)
			} else {
				generateName(Sex.Neuter,4,10,14) + chosenRoot.string
			}
			name.capitalize
		}

		/**
		 * Generate a new person name based on the language's naming system and phonemology, will not generate duplicate
		 * names (within reason). If provided, the name may be modified based on personal attributes (like hair color,
		 * father's name, etc)
		 *
		 * @param personAttributes map of personal attributes to use when creating name particularly important for
		 *                         languages that use patronymics and epithets
		 * @return a persons name(s)
		 */
		def generatePersonName (sex : Sex, personAttributes : Map[String,String]) : Name = {

			var tries = 15
			while (tries >= 0) {
				val raw = generateName (sex,3, 8, 12)
				if (! generatedPersonNames(raw) || tries == 0) {
					generatedPersonNames += raw
					return Name (raw, None, None, None)
				}
				tries -= 1
			}

			Name("jim",None,None,None)
		}
		
		def generateDeityName (sex : Sex, deityAttributes : Map[String,String]) : Name = {
			generatePersonName(sex,deityAttributes)
		}
	}

	object Language {
		val Sentinel : Language = new Language with TSentinel {
			protected def readResolve : Object = Language.Sentinel
		}
		
		def synthesize ( baseLanguages : Map[LanguageBasis,Float] ) : Language = {
			val lang = new Language
			lang.languageChains = StringMarkov.createFrom(baseLanguages.map( tup => tup._1.languageChains -> tup._2 ))
			lang.maleNameChains = StringMarkov.createFrom(baseLanguages.map( tup => tup._1.maleNameChains -> tup._2 ))
			lang.maleNameChains.fallbackChain = Some(lang.languageChains)
			lang.femaleNameChains = StringMarkov.createFrom(baseLanguages.map( tup => tup._1.femaleNameChains -> tup._2 ))
			lang.femaleNameChains.fallbackChain = Some(lang.languageChains)
			lang
		}
	}
}
