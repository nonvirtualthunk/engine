package arx.core.gen

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/13/13
 * Time: 1:46 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.mutable
import annotation.tailrec
import collection.immutable.WrappedString
import java.io.{ObjectOutput, ObjectInput, Externalizable}

@SerialVersionUID(1L)
class Markov[T](val startSentinel:T,val endSentinel:T,var orders:Int,var octaveMultiplier : Float = 0.15f) extends Serializable {
	var seed = System.currentTimeMillis()
	@transient
	var _random = new scala.util.Random(seed)
	def random = { if ( _random == null ) { _random = new scala.util.Random(seed) } ; _random }

	var prefixChain = (1 to orders).map( i => (0 until i).map(_ => startSentinel).toList ).toArray
	var links = Array.fill(orders)(new mutable.HashMap[ List[T] , MarkovLink[T] ]())
	var fallbackChain : Option[Markov[T]] = None
	protected var finished = false

	def train ( set : Traversable[Traversable[T]] ) {
		set.foreach( trainEntry )
	}
	def trainEntry ( entry : Traversable[T] ) {
		var order = 0
		while ( order < orders ) {
			val chain = (prefixChain(order) ::: entry.toList) :+ endSentinel

			for ( subChain <- chain.sliding(order+2) ) {
				val lookupChain = subChain.take(order+1)
				val outputChain = subChain.last
				val link = links(order).getOrElseUpdate(lookupChain,new MarkovLink[T])
				link.add(outputChain,1.0f)
			}

			order += 1
		}
	}

	def finishTraining () {
		if ( ! finished ) {
			finished = true

			if ( octaveMultiplier > 0.0f ) {
				for ( order <- 2 until orders ) {
					links(order).foreach {
						case (lookupChain,link) =>
							val lowerLink = links(order - 1)(lookupChain.takeRight(order))
							link.merge(lowerLink,octaveMultiplier)
					}
				}
			}
		}
	}

	@tailrec
	final def linkFor ( chain : List[T] , order : Int ) : MarkovLink[T] = {
		order match {
			case -1 => new MarkovLink[T]
			case _ => links(order).get(chain) match {
				case Some(res) => res
				case None => linkFor(chain.takeRight(order),order-1)
			}
		}
	}

	def passthroughCondition ( chain : List[T] ) = false

	/**
	 * Starts at 1
	 * @param orderFrom1
	 */
	def generate(orderFrom1 : Int) :Traversable [T] = {
		generate(Nil,passthroughCondition _,10000,orderFrom1)
	}

	def generate(startChain : List[T], endCondition : (List[T]) => Boolean , desiredLength : Int , orderFrom1 : Int) : Traversable[T] = {
		finishTraining()
		val order = orderFrom1 - 1

		var endChain = List[T]()
		var chain : List[T] = prefixChain(order) ::: startChain.filterNot(_ == startSentinel)

		while ( true ) {
			val lookupChain = chain.takeRight(order+1)
			val link = linkFor(lookupChain,order)

			var r = random.nextFloat() * link.total

			//If we're past our desired chain length, start increasing the likelihood of ending
			//we don't simply want to cut off, however, since there are certain letters that almost
			//never mark the end of a word, like, say, "j" in english
			if (endChain.length >= desiredLength) {
				val endChainChance = link.counts.getOrElse(endSentinel,0.0f).max(link.total * 0.05f)
				val effChance = (endChainChance * math.pow(2,endChain.length - desiredLength + 1)).min(link.total * 0.5f)
				if (r <= effChance) {
					return endChain
				}
			}
//			if ( endChain.length >= desiredLength && link.counts.contains(endSentinel) ) {
//				if ( r <= link.counts(endSentinel) * 2.0f ) {
//					return endChain
//				}
//			}

			link.counts.find {
				case (value,count) =>
					r -= count
					r <= 0.0f
			} match {
				case Some((v,_)) =>
					chain = chain :+ v
					if ( v != endSentinel ) {
						endChain = endChain :+ v
					}
				case None => {
					fallbackChain match {
						case Some(fallback) =>
							fallback.linkFor(lookupChain,order).counts.find {
								case (v,c) => {
									r -= c
									r <= 0.0f
								}
							} match {
								case Some((fv,_)) =>
									chain = chain :+ fv
									if ( fv != endSentinel ) {
										endChain = endChain :+ fv
									}
								case None => Noto.warn("No valid successor to chain \"" + lookupChain + "\" in fallback, returning"); return endChain
							}
						case None => Noto.warn("No valid successor to chain \"" + lookupChain + "\" and no fallback, returning"); return endChain
					}
				}
			}

			if ( chain.last == endSentinel || endCondition(chain) ) {
				return endChain
			}
		}
		Nil
	}

	def mergeIn ( other : Markov[T] , weight : Float ) {
		for ( order <- 0 until other.orders ; linkMap = other.links(order) ) {
			for ( (lookupChain,link) <- linkMap ) {
				this.links(order).getOrElseUpdate(lookupChain,new MarkovLink[T]).merge(link,weight)
			}
		}
	}
}

@SerialVersionUID(1L)
class StringMarkov(_orders:Int,_octaveMultiplier:Float=0.15f) extends Markov[Char]('$','^',_orders,_octaveMultiplier) {
	def this() {
		this(0,0.15f)
	}

	def trainString ( str : String ) {
		trainEntry(str)
	}
	def trainStrings ( set : Traversable[String] ) {
		set.foreach( str => trainEntry(str) )
	}

	val sb = new StringBuilder
	def buildString ( chars : Traversable[Char] ) = {
		sb.clear()
		for ( char <- chars ) {
			sb.append(char)
		}
		sb.toString
	}

	override def generate( orderFrom1 : Int) : WrappedString = {
		buildString(super.generate(orderFrom1))
	}


	override def generate(startChain : List[Char], endCondition : (List[Char]) => Boolean , desiredMaxLength : Int, orderFrom1 : Int) : WrappedString = {
		buildString(super.generate(startChain,endCondition,desiredMaxLength,orderFrom1))
	}
	/*
		var seed = System.currentTimeMillis()
		@transient
		var _random = new scala.util.Random(seed)
		def random = { if ( _random == null ) { _random = new scala.util.Random(seed) } ; _random }

		val prefixChain = (1 to orders).map( i => (0 until i).map(_ => startSentinel).toList ).toArray
		val links = Array.fill(orders)(new mutable.HashMap[ List[T] , MarkovLink ]())
		private var finished = false

	 */

	def currentVersion = 2
//	def writeExternalVersioned(out: ArxOutputStream) {
//		finishTraining()
//
//		out.writeLong(seed)
//		out.writeBoolean(finished)
//		out.writeInt(orders)
//		out.writeFloat(octaveMultiplier)
//
//		for ( order <- 0 until orders ) {
//			val linkMap = links(order)
//			out.writeShort(linkMap.size)
//
//			var allKeysString = ""
//			for ( k <- linkMap.keys ) {
//				for ( c <- k ) { allKeysString += c }
//			}
//			out.writeUTF8(allKeysString)
//
//			var index = 0
//			for ( k <- linkMap.keys) {
//				val v = linkMap(k)
////				for ( c <- k ) {
////					out.writeChar(c)
////				}
//				if ( k.foldLeft("")(_ + "" + _) != allKeysString.substring(index,index+order+1) ) {
//					Noto.warn("BADBAD, should be : " + k + " but was : " + allKeysString.substring(index,index+order+1))
//				}
//				index += order + 1
//
//				out.writeFloat( v.total )
//				out.writeShort( v.counts.size )
//
//				var charString = ""
//				for ( countKey <- v.counts.keys ) {
////					out.writeChar(countKey)
//					charString += countKey
//				}
//				out.writeUTF8(charString)
//				for ( countValue <- v.counts.values ) {
//					out.writeFloat(countValue)
//				}
//			}
//		}
//
//		out.write(fallbackChain)
//	}
//	def readExternalVersioned(version: Int, in: ArxInputStream) {
//		if ( version == 1 ) {
//			seed = in.readLong
//			_random = new scala.util.Random(seed)
//			finished = in.readBoolean
//			orders = in.readInt
//			octaveMultiplier = in.readFloat
//			prefixChain = (1 to orders).map( i => (0 until i).map(_ => startSentinel).toList ).toArray
//
//			links = Array.fill(orders)(new mutable.HashMap[ List[Char] , MarkovLink[Char] ]())
//			for ( order <- 0 until orders ) {
//				val linkMap = links(order)
//				val linkMapSize = in.readShort
//
//				var index = 0
//				val allKeys = in.readUTF8
//				for ( i <- 0 until linkMapSize ) {
//					val linkKey = allKeys.substring(index,index+order+1).toList
//					index += order + 1
//
//					val link = new MarkovLink[Char]
//					link.total = in.readFloat
//					val countsSize = in.readShort
//
//	//				var keys = List[Char]()
//	//				q = 0; while ( q < countsSize ) {
//	//					keys ::= in.readChar
//	//					q += 1
//	//				}
//					val keys = in.readUTF8 : Iterable[Char]
//					var values = List[Float]()
//					var q = 0; while ( q < countsSize ) {
//						values ::= in.readFloat
//						q += 1
//					}
//
//					link.counts = keys.zip(values).toMap
//
//					linkMap.put(linkKey,link)
//				}
//			}
//		} else if ( version == 2 ) {
//			readExternalVersioned(1,in)
//			fallbackChain = in.read
//		}
//	}
}

object StringMarkov {
	def createFrom ( bases : Map[StringMarkov,Float] ) = {
		val markov = new StringMarkov(bases.map(_._1.orders).max,0.0f)
		for ( (basis,weight) <- bases ) {
			markov.mergeIn(basis,weight)
		}
		markov
	}

	val Sentinel : StringMarkov = new StringMarkov(3) {
		override def generate(orderFrom1: Int): WrappedString = "Sentinel"
		protected def readResolve : Object = StringMarkov.Sentinel
	}
}

class MarkovLink[T] extends Serializable {
	var counts = Map[T,Float]()
	var total = 0.0f
	def add ( value : T , incr : Float ) {
		counts += value -> (counts.getOrElse(value,0.0f) + incr)
		total += incr
	}
	def merge ( other : MarkovLink[T] , multiplier : Float ) {
		for ( (v,count) <- other.counts ) {
			add(v,count * multiplier)
		}
	}
}