package arx.core.language.semantic

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 6/22/14
 * Time: 1:28 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.traits.{TSentinel, TSentinelable}
import scalaxy.loops._

object Grammar {
	object Tense extends Enumeration {
		type Tense = Value
		val None, Present, Imperfect, Past, Future, FuturePerfect, Pluperfect, ActiveParticiple, PastParticiple, Infinitive = Value

		lazy val map = this.values.map (v => v.toString.toLowerCase.stripWhitespace -> v).toMap

		def fromString(str: String) = map.getOrElse (str.toLowerCase.stripWhitespace, {
				Noto.warn (f"Unknown tense '$str'")
				None
		})

		def isTense(str: String) = map.contains (str)
	}

	object Person extends Enumeration {
		type Person = Value
		val None, First, Second, Third = Value

		lazy val map = Map("first" -> First,"second" -> Second,"third" -> Third,
			"1" -> First,"2" -> Second,"3" -> Third,
			"1st" -> First,"2nd" -> Second,"3rd" -> Third)

		def fromString(str: String) = map.getOrElse (str.toLowerCase.stripWhitespace, {
			Noto.warn (f"Unknown person '$str'")
			None
		})

		def isTense(str: String) = map.contains (str)
	}

	object Voice extends Enumeration {
		type Voice = Value
		val Active, Passive = Value
		lazy val map = this.values.map (v => v.toString.toLowerCase.stripWhitespace -> v).toMap

		def fromString(str: String) = map.getOrElse (str.toLowerCase, {
			Noto.warn (f"Unknown tense '$str'")
			None
		})

		def isTense(str: String) = map.contains (str)
	}

	abstract class WordForm extends TSentinelable { def forPartOfSpeech : PartOfSpeech.Value }
	case class VerbForm (person : Person.Value,tense : Tense.Value,singular : Boolean, voice : Voice.Value = Voice.Active) extends WordForm {
		override def forPartOfSpeech: PartOfSpeech.Value = PartOfSpeech.Verb
	}
	object PastParticiple extends VerbForm (Person.First,Tense.PastParticiple,singular = true)
	object ActiveParticiple extends VerbForm (Person.First,Tense.ActiveParticiple,singular = true)
	object InfinitiveForm extends VerbForm (Person.First,Tense.Infinitive,singular = true)

	case class NounForm (singular : Boolean, definiteness : Definity.Value = Definity.None) extends WordForm {
		override def forPartOfSpeech: PartOfSpeech.Value = PartOfSpeech.Noun
	}
	case class AdjectiveForm (comparison : AdjectiveType.Value) extends WordForm {
		override def forPartOfSpeech: PartOfSpeech.Value = PartOfSpeech.Adjective
	}
	object PositiveAdjective extends AdjectiveForm(AdjectiveType.Positive)
	object ComparativeAdjective extends AdjectiveForm(AdjectiveType.Comparative)
	object SuperlativeAdjective extends AdjectiveForm(AdjectiveType.Superlative)
	object AdverbForm extends WordForm {
		override def forPartOfSpeech: PartOfSpeech.Value = PartOfSpeech.Adverb
	}
	object PrepositionForm extends WordForm {
		override def forPartOfSpeech: PartOfSpeech.Value = PartOfSpeech.Preposition
	}
	object InstanceForm extends NounForm(true,Definity.Indefinite) {}
	object DefiniteForm {
		def apply (singular : Boolean) = NounForm(singular,Definity.Definite)
	}


	val AllVerbForms = (for (person <- Person.values ; tense <- Tense.values ; singular <- Set(true,false) ; voice <- Voice.values ; if person != Person.None && tense != Tense.None) yield {
		VerbForm(person,tense,singular,voice)
	}).toList

	object WordForm {
		protected object sentinel extends WordForm with TSentinel {
			override def forPartOfSpeech: PartOfSpeech.Value = PartOfSpeech.Unknown
		}
		val Sentinel : WordForm = sentinel
		def defaultWordFormForPartOfSpeech(pos : PartOfSpeech.Value) = {
			pos match {
				case PartOfSpeech.Noun => NounForm(singular = true)
				case PartOfSpeech.Verb => VerbForm(Grammar.Person.First,Grammar.Tense.Present,singular = true)
				case PartOfSpeech.Adjective => AdjectiveForm(AdjectiveType.Positive)
				case PartOfSpeech.Adverb => AdverbForm
				case PartOfSpeech.Preposition => PrepositionForm
				case _ =>
					Noto.error("Invalid part of speech : " + pos)
					Sentinel
			}
		}
	}

	object PartOfSpeech extends Enumeration {
		type PartOfSpeech = Value
		val Unknown, Noun, Verb, Adjective, Adverb, Preposition = Value


		lazy val map = this.values.map(v => v.toString.toLowerCase.stripWhitespace -> v).toMap
		def fromString (str : String) = map.getOrElse(str.toLowerCase.stripWhitespace,{
			Noto.warn(f"Unknown part of speech'$str'");Unknown })
		def isPartOfSpeech (str : String) = map.contains(str)
	}

	object AdjectiveType extends Enumeration {
		type AdjectiveType = Value
		val Positive,Comparative,Superlative = Value
	}

	object Definity extends Enumeration {
		type Definity = Value
		val Definite,Indefinite,None = Value
	}


	val VerbFormRedirects = {
		var ret = Map[VerbForm,VerbForm]()
		for (person <- Person.values; tense <- Tense.values; singular <- List(true,false); voice <- Voice.values){
			val vf = VerbForm(person,tense,singular,voice)

			var mapTo = vf

			mapTo = mapTo.copy(singular = false) // english verbs don't ever seem to vary by plurality (except irregular)
			if (voice == Voice.Passive ) {
				mapTo = mapTo.copy(tense = Tense.PastParticiple,voice = Voice.Active)
			}
			if (person != Person.Third || tense != Tense.Present) {
				mapTo = mapTo.copy(person = Person.First)
			}
			if (tense == Tense.Pluperfect) {
				mapTo = mapTo.copy(tense = Tense.PastParticiple)
			} else if (tense == Tense.Imperfect && voice == Voice.Active) {
				mapTo = mapTo.copy(tense = Tense.ActiveParticiple)
			} else if (tense == Tense.FuturePerfect) {
				mapTo = mapTo.copy(tense = Tense.PastParticiple)
			} else if (tense == Tense.Future) {
				mapTo = mapTo.copy(tense = Tense.Present)
			}

			if (mapTo != vf) {
				ret += vf -> mapTo
			}
		}
		ret
	}

/*
						active					passive
Pluperfect		had killed				had been killed
Past				killed					was killed
Imperfect		was killing				was being killed
Present			kill						am being killed
Future			will kill				will be killed
FuturePerfect	will have killed		will have been killed

I was			we were
you were		you were
he was		they were


 */

	val helperVerbFor = AllVerbForms.zip(AllVerbForms.map {
			// future perfect passive
		case VerbForm(_,Tense.FuturePerfect,_,Voice.Passive) => "will have been"
			// future passive
		case VerbForm(_,Tense.Future,_,Voice.Passive) => "will be"
			// present passive
		case VerbForm(Person.First,Tense.Present,true,Voice.Passive) => "am being"
		case VerbForm(Person.First,Tense.Present,false,Voice.Passive) => "are being"
		case VerbForm(Person.Third,Tense.Present,true,Voice.Passive) => "is being"
		case VerbForm(Person.Third,Tense.Present,false,Voice.Passive) => "are being"
		case VerbForm(Person.Second,Tense.Present,_,Voice.Passive) => "are being"
			// imperfect passive
		case VerbForm(Person.First,Tense.Imperfect,true,Voice.Passive) => "was being"
		case VerbForm(Person.Third,Tense.Imperfect,true,Voice.Passive) => "was being"
		case VerbForm(_			  ,Tense.Imperfect,_,Voice.Passive) => "were being"
			// past passive
		case VerbForm(Person.First,Tense.Past,true,Voice.Passive) => "was"
		case VerbForm(Person.Third,Tense.Past,true,Voice.Passive) => "was"
		case VerbForm(_			  ,Tense.Past,_,Voice.Passive) => "were"
			// pluperfect passive
		case VerbForm(Person.Second,Tense.Pluperfect,true,Voice.Passive) => "have been"
		case VerbForm(_,Tense.Pluperfect,true,Voice.Passive) => "had been"
		case VerbForm(_,Tense.Pluperfect,false,Voice.Passive) => "have been"
			
			// future perfect active
		case VerbForm(_,Tense.FuturePerfect,_,Voice.Active) => "will have"
			// future active
		case VerbForm(_,Tense.Future,_,Voice.Active) => "will"
			// present active
		case VerbForm(_,Tense.Future,_,Voice.Active) => ""
			// imperfect active
		case VerbForm(Person.First,Tense.Imperfect,true,Voice.Active) => "was"
		case VerbForm(Person.Third,Tense.Imperfect,true,Voice.Active) => "was"
		case VerbForm(_			  ,Tense.Imperfect,_,Voice.Active) => "were"
			// present active
		case VerbForm(_,Tense.Present,_,Voice.Active) => ""
			// past active
		case VerbForm(_,Tense.Past,_,Voice.Active) => ""
			// pluperfect active
		case VerbForm(_,Tense.Pluperfect,_,Voice.Active) => "had"
			// fallback, should not match anything
		case VerbForm(_,Tense.Infinitive,_,_) => "to"
		case VerbForm(_,Tense.ActiveParticiple,_,_) => ""
		case VerbForm(_,Tense.PastParticiple,_,_) => ""
		case o => Noto.warn(f"Uncaught case when determining helper verb : $o")
	}).toMap

	def defaultVerbFormFor(entry: SemanticEntry, t: VerbForm): String = {
		val partOfSpeech = entry.partOfSpeech
		val coreWord = entry.coreWord

		t.voice match {
			case Voice.Passive =>
				coreWord + "ed"
			case Voice.Active => {
				partOfSpeech match {
					case PartOfSpeech.Verb =>
						t.person match {
							case Grammar.Person.Third =>
								t.tense match {
									case Tense.Present =>
										if (coreWord.endsWith("y")) { coreWord.dropRight(1) + "ies" }
										else { coreWord + "s" }
									case _ => entry.verbFormFor(t.copy(Grammar.Person.First))
								}
							case _ =>
								t.tense match {
									case Tense.Present | Tense.Future =>
										coreWord
									case Tense.Past | Tense.PastParticiple =>
										if (coreWord.endsWith("e")) { coreWord + "d" }
										else { coreWord + "ed" }
									case Tense.Pluperfect =>
										entry.verbFormFor(t.copy(tense = Grammar.Tense.Past))
									case Tense.ActiveParticiple =>
										if (coreWord.endsWith("e")) { coreWord.dropRight(1) + "ing" }
										else { coreWord + "ing" }
								}
						}
					case _ =>
						Noto.warn("No way to implicitly make a verb from a(n) " + partOfSpeech)
						""
				}
			}
		}
	}

	def defaultNounFormFor(entry:SemanticEntry, t : NounForm) : String = {
		val word = entry.coreWord
		entry.partOfSpeech match {
			case PartOfSpeech.Noun =>
				t match {
					case NounForm (singular,definity) =>
						val mainWord = singular match {
							case true => word
							case false =>
								if (word.endsWith ("s")) {
									word + "es"
								} else if (word.endsWith ("o")) {
									word + "es"
								} else if (word.endsWith ("y")) {
									word.dropRight (1) + "ies"
								} else {
									word + "s"
								}
						}
						val prefix = definity match {
							case Definity.None => ""
							case Definity.Definite => "the "
							case Definity.Indefinite => {
								entry.wordKind.headOption.getOrElse(WordKind.Unknown) match {
									case WordKind.Material => "a piece of " + entry.coreWord
									case WordKind.Liquid => "a drop of " + entry.coreWord
									case _ => "a " + entry.coreWord
								}
							}
						}
						prefix + mainWord
				}
			case _ =>
				Noto.warn("No way to implicitly make a noun from a(n) " + entry.partOfSpeech)
				""
		}
	}
}