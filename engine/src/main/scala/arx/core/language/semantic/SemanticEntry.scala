package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/29/14
 * Time: 8:39 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.language.semantic.Grammar._
import arx.core.representation.{ConfigValue, TConfigurable}
import arx.core.traits.{TSentinelable, TSentinel}

import scala.collection.mutable
import arx.core.representation.ConfigUtil._

//class SemanticEntry {}

class SemanticEntry (val coreWord : String,val partOfSpeech : PartOfSpeech.Value,val disambiguator : String) extends TSentinelable with TConfigurable with SemanticEntryPointer {
	protected var _derivedFrom : Option[SemanticEntry] = None
	def derivedFrom = _derivedFrom
	def derivedFrom(e : SemanticEntry) {
		_derivedFrom = Some(e)
		quantities = e.quantities
	}

	var meanings : List[SemanticMeaning] = Nil
	var wordKind = Set[WordKind.Value]()
	var links : List[SemanticLink] = Nil
	var quantities : Map[String,Float] = Map().withDefault(_ => 0.0f)
	var qualities : List[SemanticQuality] = Nil
	protected var _wordForms = new mutable.HashMap[Grammar.WordForm,String]
	protected var _partOfSpeechVariations = new mutable.HashMap[PartOfSpeech.Value,mutable.Set[SemanticEntry]] with mutable.MultiMap[PartOfSpeech.Value,SemanticEntry]

	def connotation : Int = quantities.getOrElse(SemanticLink.Connotation,0.0f).toInt
	def connotation_= (c : Int) { quantities += (SemanticLink.Connotation -> c) }
	def specificity : Int = quantities.getOrElse(SemanticLink.Specificity,0.0f).toInt
	def specificity_= (c : Int) { quantities += (SemanticLink.Specificity -> c) }
	def associations = links.filter(_.kind == SemanticLink.Association).map(_.to)
	def synonyms = links.filter(_.kind == SemanticLink.Synonym).map(_.to)
	def antonyms = links.filter(_.kind == SemanticLink.Antonym).map(_.to)
	def properties = qualities.filter(_.kind == SemanticQuality.Property).map(_.quality)
	def hasProperty (prop : String) = properties.contains(prop)

	def hasLink (kind : String, to: String) : Boolean = { links.exists( l => l.kind =~= kind && l.to.coreWord =~= to ) }
	def hasLink (kind : String) : Boolean = { links.exists( _.kind =~= kind ) }

	def hasWordForm (form : Grammar.WordForm) : Boolean = _wordForms.contains(form)
	def addWordForm (form : Grammar.WordForm, str : String) { _wordForms.put(form,str) }

	protected def newPartOfSpeechVariation (word : String, pos : PartOfSpeech.Value) = {
		val disambig = if ( word == coreWord ) { Some(pos.toString) } else { None }
		val entry = SemanticDictionary.entryFor(word,pos,Some(this),disambig)
		entry._partOfSpeechVariations.addBinding(this.partOfSpeech,this)
		_partOfSpeechVariations.addBinding(pos,entry)

		entry
	}
	def rootEntry : SemanticEntry = derivedFrom match { case Some(df) => df.rootEntry ; case None => this }
	def isRoot = derivedFrom.isEmpty
	def verbFormFor (form : Grammar.VerbForm) : Word = {
		if (this.partOfSpeech != PartOfSpeech.Verb) {
			_partOfSpeechVariations.get(PartOfSpeech.Verb) match {
				case Some(verbSet) => verbSet.head.verbFormFor(form)
				case None => Noto.warn(s"Verb form requested from non-verb with no conversion : $coreWord");Word.Sentinel
			}
		} else {
			val str = _wordForms.get(form) match {
				case Some (res) => res
				case None => {
					val redirectedForm = Grammar.VerbFormRedirects.getOrElse(form,form)
					_wordForms.getOrElseUpdate(redirectedForm,Grammar.defaultVerbFormFor (this,form))
				}
			}
			Word(str,this,form)
		}
	}
	def nounFormFor (form : Grammar.NounForm) : Word = {
		if (this.partOfSpeech != PartOfSpeech.Noun) {
			_partOfSpeechVariations.get(PartOfSpeech.Noun) match {
				case Some(nounSet) => nounSet.head.nounFormFor(form)
				case None => Noto.warn(s"Noun form requested from non-noun with no conversion : $coreWord"); Word.Sentinel
			}
		} else {
			val str = _wordForms.getOrElseUpdate(form, Grammar.defaultNounFormFor(this,form))
			Word(str,this,form)
		}
	}
	def adjectiveForm (targetForm : Grammar.AdjectiveForm) : Word = {
		_wordForms.get(targetForm) match {
			case Some(res) => Word(res,this,targetForm)
			case None => {
				partOfSpeech match {
					case PartOfSpeech.Adjective => {
						if (targetForm.comparison == AdjectiveType.Positive) {
							Word(coreWord,this,targetForm)
						} else {
							derivedFrom match {
								case Some(parent) if parent.partOfSpeech == PartOfSpeech.Verb => Word.Sentinel // valid to try, but can't do
								case _ => {
									val str = targetForm.comparison match {
										case AdjectiveType.Comparative => coreWord + "er"
										case AdjectiveType.Superlative => coreWord + "est"
									}
									Word(str,this,targetForm)
								}
							}
						}
					} case _ => {
						_partOfSpeechVariations.get(PartOfSpeech.Adjective) match {
							case Some(adjSet) => adjSet.head.adjectiveForm(targetForm)
							case None => {
								if (partOfSpeech == PartOfSpeech.Verb){
									val gerund = verbFormFor(Grammar.ActiveParticiple)
									val entry = newPartOfSpeechVariation(gerund,PartOfSpeech.Adjective)
									entry.adjectiveForm(targetForm)
								} else {
									Noto.warn("No implicit way to construct an adjective from a : " + this.partOfSpeech)
									Word.Sentinel
								}
							}
						}
					}
				}
			}
		}
	}

	def pluralForm = nounFormFor(Grammar.NounForm(singular = false))

	def resolveEntry = this
	
	protected val formFromKey : PartialFunction[String,Grammar.WordForm] = {
		case "instance" => InstanceForm
		case "plural" => NounForm(singular = false)
		case "comparative" => Grammar.ComparativeAdjective
		case "superlative" => Grammar.SuperlativeAdjective
		case s if Tense.isTense(s.toLowerCase.stripWhitespace) => Grammar.VerbForm(Grammar.Person.First,Tense.fromString(s),singular = true)
	}

	protected def toLinks (sml : List[String],kind : String) = sml.map( s => SemanticLink(this,kind,s)).toList
	protected def toQualities (sml : List[String],kind : String) = sml.map( s => SemanticQuality(this,kind,s)).toList
	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		implicit def toPointers (l : List[String]) = l.map(new DictionaryPointer(_))
		meanings = extractStrings(sml,"meaning","meanings").map(SemanticMeaning.fromString)
		wordKind = extractStrings(sml,"kind","kinds").map(WordKind.fromString).toSet
		links :::= toLinks(extractStrings(sml,"synonym","synonyms"),SemanticLink.Synonym)
		links :::= toLinks(extractStrings(sml,"antonym","antonyms"),SemanticLink.Antonym)
		links :::= toLinks(extractStrings(sml,"association","associations"),SemanticLink.Association)
		qualities :::= toQualities(extractStrings(sml,"property","properties"),SemanticQuality.Property)
		if (sml.positivity.nonEmpty) { connotation = sml.positivity.int }
		else { connotation = sml.positive.int }
		specificity = sml.specificity.intOrElse(specificity)
		for ((key,value) <- sml.links.fields) {
			for (v <- value.arr) {
				links ::= SemanticLink (this, key.toLowerCase.stripWhitespace, new DictionaryPointer(v.str))
			}
		}
		for ((key,value) <- sml.quantity.fields ++ sml.quantities.fields) {
			quantities += key -> value.float
		}


		for ((key,value_base) <- sml.forms.fields ; values = value_base.arr ; value_under <- values ; value = value_under.str ) {
			if (PartOfSpeech.isPartOfSpeech(key)) {
				newPartOfSpeechVariation(value,PartOfSpeech.fromString(key))
			} else if (formFromKey.isDefinedAt(key)) {
				val newForm = formFromKey(key)
				addWordForm(newForm,value)
			} else if (key.startsWith("to")) { // assume this is something like, for "hard", { toMake : "harden" }
				val meaningParts = key.fromCamelCase.split(" ")
				if (meaningParts.length > 1) {
					val verb = meaningParts(1)
					val disambig = if(value == coreWord) { Some(verb) } else { None }
					val derived = SemanticDictionary.entryFor(value,PartOfSpeech.Verb,Some(this),disambig)
					derived.meanings = List(new VerbPhrase(new DictionaryPointer(verb),this))
				} else {
					Noto.warn(f"unknown form kind : $key,$value")
				}
			} else {
				Noto.warn(f"unknown 'form' value in semantic dictionary entry, key is : $key")
			}
		}
	}

	override def toString: String = coreWord
}



object EmptySemanticEntry extends SemanticEntry ("",PartOfSpeech.Unknown,"Sentinel") with TSentinel {
	
}