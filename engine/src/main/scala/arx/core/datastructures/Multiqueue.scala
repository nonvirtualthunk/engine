package arx.core.datastructures

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 5/17/15
 * Time: 8:50 AM
 */

import arx.Prelude._
import scalaxy.loops._

/**
 * A single write, multiple read queue that tracks each partition separately.
 */
class Multiqueue[T] {
	var intern = Vector[T]()
	var indices = Array.ofDim[Long](0)
	var baseIndex = 0l
	protected var compactionCounter = 0

	def enqueue(t : T): Unit = synchronized {
		intern :+= t
	}
	def enqueue(ts : Iterable[T]): Unit = synchronized {
		for (t <- ts) {
			intern :+= t
		}
	}

	protected def createConsumerKey : MultiqueueConsumerKey = synchronized {
		val key = MultiqueueConsumerKey(indices.length)
		val newIndices = Array.ofDim[Long](indices.length + 1)
		for (i <- 0 until indices.length optimized) {
			newIndices(i) = indices(i)
		}
		indices = newIndices
		indices(key.index) = baseIndex
		key
	}
	
	def createConsumer = {
		val key = createConsumerKey
		new MultiqueueConsumer[T](this, key)
	}

	def dequeue(consumer : MultiqueueConsumerKey) = synchronized {
		val vec = intern

		val targetIndex = indices(consumer.index)
		val offset = targetIndex - baseIndex
		if (vec.size > offset) {
			indices(consumer.index) = targetIndex+1
			checkAutomaticCompaction()

			Some(vec(offset.toInt))
		} else {
			None
		}
	}

	/** Automatically performs a compaction operation every 128th successful dequeue */
	protected def checkAutomaticCompaction (): Unit = {
		compactionCounter = (compactionCounter + 1) & 127
		if (compactionCounter == 0) {
			compact()
		}
	}

	def compact (): Unit = synchronized {
		val minIndex = indices.min
		if (minIndex > baseIndex) {
			intern = intern.drop((minIndex - baseIndex).toInt)
			baseIndex = minIndex
		}
	}
}

class MultiqueueConsumer[T](parent : Multiqueue[T], key : MultiqueueConsumerKey) {
	def dequeue () = parent.dequeue(key)
	def dequeueAll () : List[T] = {
		var ret = List[T]()
		var tmp : Option[T] = dequeue()
		while (tmp.nonEmpty) {
			ret ::= tmp.get

			tmp = dequeue()
		}
		ret
	}
	
	def offset = parent.baseIndex + parent.indices(key.index)
}

case class MultiqueueConsumerKey (index : Int)


/** A multiqueue that also tracks a per-partition map of sub-offsets. These sub offsets are not given a
  * context here, they have no inherent meaning. In the initial implementation use case, they represent
  * the lastUpdated mark point of a given talea.
  */
class BiLevelMultiqueue[T] extends Multiqueue[T] {
	var subOffsets = Array.ofDim[Map[T,Int]](0)


	override protected def createConsumerKey: MultiqueueConsumerKey = synchronized {
		val ret = super.createConsumerKey

		val tmp = Array.ofDim[Map[T,Int]](indices.length)
		for (i <- 0 until subOffsets.length optimized) {
			tmp(i) = subOffsets(i)
		}
		tmp(tmp.length - 1) = Map[T,Int]().withDefaultValue(0)
		subOffsets = tmp

		ret
	}

	override def createConsumer: BiLevelMultiqueueConsumer[T] = {
		val key = createConsumerKey
		new BiLevelMultiqueueConsumer[T](this, key)
	}

	def subOffsetFor (key : MultiqueueConsumerKey, x : T) = subOffsets(key.index)(x)
	def updateSubOffsetFor (key : MultiqueueConsumerKey, x : T, off : Int) {
		subOffsets(key.index) += x -> off
	}
}

class BiLevelMultiqueueConsumer[T](parent : BiLevelMultiqueue[T], key : MultiqueueConsumerKey) extends MultiqueueConsumer[T](parent,key){
	def subOffsetFor (x : T) = parent.subOffsetFor(key,x)
	def updateSubOffsetFor (x : T, off : Int) { parent.updateSubOffsetFor(key,x,off) }
}