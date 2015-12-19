package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/21/14
 * Time: 9:21 AM
 */

import java.util.regex.Pattern


import arx.Prelude
import arx.Prelude._
import arx.application.Noto
import arx.core.language.semantic.Grammar._
import arx.core.representation.{ConfigValue, TConfigurable}
import arx.core.traits.{TSentinel, TSentinelable}
import arx.resource.ResourceManager

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

trait TDictionarySubset extends Traversable[SemanticEntry] {
	def allEntries : Traversable[SemanticEntry]

	protected def passthrough (b : Int) = true

	def withPartOfSpeech(p : PartOfSpeech.Value) = DictionarySubset(allEntries.filter(_.partOfSpeech == p))
	def withWordKind(k : WordKind.Value) = DictionarySubset(allEntries.filter(_.wordKind(k)))
	def withProperty(p : String) = DictionarySubset(allEntries.filter(_.properties.contains(p)))
	def withLink(kind:String,to:String) = DictionarySubset(allEntries.filter(_.hasLink(kind,to)))
	def withPositivity(n : (Int) => Boolean) = DictionarySubset(allEntries.filter(e => n(e.connotation)))
	def withSpecificity(n : (Int) => Boolean) = DictionarySubset(allEntries.filter(e => n(e.specificity)))
	def withQuantity(quantity : String, n : (Float) => Boolean) = DictionarySubset(allEntries.filter(e => n(e.quantities(quantity))))
	def rootEntries = DictionarySubset(allEntries.filter(_.derivedFrom.isEmpty))
	def select (partOfSpeech : PartOfSpeech.Value = PartOfSpeech.Unknown,wordKind : WordKind.Value = WordKind.Unknown,
					  positivity : (Int) => Boolean = passthrough, specificity : (Int) => Boolean = passthrough,
					  quantity : (String,(Float) => Boolean) = ("",(f:Float) =>true)) = {
		var ret = DictionarySubset(allEntries)
		if (partOfSpeech != PartOfSpeech.Unknown) { ret = ret.withPartOfSpeech(partOfSpeech) }
		if (wordKind != WordKind.Unknown) { ret = ret.withWordKind(wordKind) }
		if (positivity != passthrough _) {
			ret = ret.withPositivity(positivity)
		}
		if (specificity != passthrough _) {
			ret = ret.withSpecificity(specificity)
		}
		if (quantity._1 != "") {
			ret = ret.withQuantity(quantity._1,quantity._2)
		}
		ret
	}
	def rand = Prelude.randFrom(allEntries.toSeq)

	def ++ (t : TDictionarySubset) = DictionarySubset((allEntries ++ t.allEntries).toSet)
	def ++ (t : Traversable[SemanticEntry]) = DictionarySubset((allEntries ++ t).toSet)
	override def filter (c : (SemanticEntry) => Boolean) = DictionarySubset(allEntries.filter(c))
	override def filterNot (c : (SemanticEntry) => Boolean) = DictionarySubset(allEntries.filterNot(c))
	def mapEntries (c : (SemanticEntry) => SemanticEntry) = DictionarySubset(allEntries.map(c))

	override def foreach[U](f: (SemanticEntry) => U): Unit = {allEntries.foreach(f)}
	override def size = allEntries.size
}

object SemanticDictionary extends TDictionarySubset {
	var allEntries = Vector[SemanticEntry]()
//	val entriesByString = new mutable.HashMap[String,mutable.Set[Int]] () with mutable.MultiMap[String,Int]
//	val entries = new mutable.HashMap[(String,PartOfSpeech.Value),Int]()

	val entries = new mutable.HashMap[String,mutable.Set[Int]] () with mutable.MultiMap[String,Int]
	protected def entries (word : String, partOfSpeech : PartOfSpeech.Value) : Set[Int] = entries.get(word) match {
		case Some(set) => set.filter(i => allEntries(i).partOfSpeech == partOfSpeech).toSet
		case None => Set[Int]()
	}

	def entryFor ( word : String, partOfSpeech : PartOfSpeech.Value ) : SemanticEntry = entryFor(word,partOfSpeech,None,None)
	def entryFor ( word : String, partOfSpeech : PartOfSpeech.Value, derivedFrom : SemanticEntry ) : SemanticEntry = entryFor(word,partOfSpeech,Some(derivedFrom),None)

	def entryFor(word:String,partOfSpeech:PartOfSpeech.Value,derivedFrom:Option[SemanticEntry],disambig : Option[String]): SemanticEntry = {
		word match {
			case "n/a" => EmptySemanticEntry
			case _ => {
				val indexSet = entries(word,partOfSpeech)
				// not filtering by the derivation right now, makes it hard to match up derivations
				// that have their own definition blocks
				val derivedFilteredIndices = indexSet //indexSet.filter( i => derivedFrom.isEmpty || derivedFrom == allEntries(i).derivedFrom )
				val matchingIndices = derivedFilteredIndices.filter( i => disambig.isEmpty || disambig.get == allEntries(i).disambiguator )

				if (matchingIndices.size > 1) {
					Noto.warn(s"Ambiguous semantic entry reference ($word,$partOfSpeech,$derivedFrom,$disambig)")
					allEntries(matchingIndices.min)
				} else if (matchingIndices.size == 1){
					allEntries(matchingIndices.head)
				} else { // no matching indices
					val newEntry = new SemanticEntry(word,partOfSpeech,disambig.getOrElse(""))
					derivedFrom match { case Some(d) => newEntry.derivedFrom(d) ; case None => }
					val index = allEntries.size
					allEntries :+= newEntry
					entries.addBinding(word,index)
					newEntry
				}
			}
		}
	}
	
	def guessEntryFor (word : String) = {
		entries.get(word) match {
			case None =>
				Noto.warn(s"No entry found for word : $word")
				EmptySemanticEntry
			case Some(set) =>
				set.size match {
					case 0 => Noto.warn(s"No entry found for word : $word");EmptySemanticEntry
					case 1 => allEntries(set.head)
					// this will be taking the first created, we assume that is the most basic
					case more => Noto.warn(s"Ambiguous reference to semantic entry with word: $word");allEntries(set.min)
				}
		}
	}

	{
		val pattern = Pattern.compile("(\\(.*?\\))")
		val sml = ResourceManager.sml("axis/language/semantic/Dictionary.conf")
		val entrySMLPairs = for ((k,v) <- sml.words.fields) yield {
			try {
				var word = k
				var disambiguator : Option[String] = None
				k.extract(pattern) match {
					case Nil =>
					case list => {
						for ( l <- list.headOption ) {
							disambiguator = Some(l)
						}
						word = word.substring(0,k.indexOf("(")).stripWhitespace
					}
				}

				val pos = PartOfSpeech.fromString(v.part.str)
				entryFor(word,pos,None,disambiguator) -> v
			} catch {
				case e : Exception =>
					Noto.warn(f"exception encountered while attempting to parse semantic entry : $k, $e")
					EmptySemanticEntry -> v
			}
		}
		for ((entry,v) <- entrySMLPairs) {
			try {
				entry.setFromSML (v)
			} catch {
				case e : Exception =>
					Noto.warn(f"Exception encountered while attempting to parse semantic entry : ${entry.coreWord}, $e, ${e.getStackTraceString}")
			}
		}
	}

	override def size = entries.size
}

class DictionarySubset(val allEntries: Traversable[SemanticEntry]) extends TDictionarySubset {}
object DictionarySubset {
	implicit def fromEntries (entries : Traversable[SemanticEntry]) = DictionarySubset(entries)
	def apply (entries : Traversable[SemanticEntry]) = new DictionarySubset(entries.filter(_.notSentinel))
}








class Word (val string : String, val semantic : SemanticEntry, val form : WordForm) extends TSentinelable {
	def rootEntry: SemanticEntry = semantic.rootEntry


	override def toString = string
}

object Word {
	protected object sentinel extends Word("",EmptySemanticEntry,WordForm.Sentinel)
	val Sentinel : Word = sentinel
	implicit def toStr( word : Word) = word.toString
//	implicit def fromSem( sem : SemanticEntry ) = new Word(sem.coreWord,sem)
//	implicit def fromStr( word : String) = new Word(word,SemanticDictionary.guessEntryFor(word))

	def apply (str : String,sem : SemanticEntry,form : WordForm) = {
		new Word(str,sem,form)
	}
}


class DictionaryPointer (val coreWord : String) extends SemanticEntryPointer {
	def resolveEntry : SemanticEntry = _entry
	lazy val _entry = SemanticDictionary.guessEntryFor(coreWord)
}



object WordKind extends Enumeration {
	type WordKind = Value
	val Unknown, Object, LivingThing, Intransitive, Transitive, Quality,
	Enumeration, Material, Liquid, State, Place, Preposition = Value
	
	
	lazy val map = this.values.map(v => v.toString.toLowerCase.stripWhitespace -> v).toMap
	def fromString (str : String) = map.getOrElse(str.toLowerCase.stripWhitespace,{
		Noto.warn(f"Unknown word kind '$str'");Unknown })
}