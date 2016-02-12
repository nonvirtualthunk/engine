package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/7/12
 * Time: 2:05 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.util.concurrent.{ConcurrentMap => JConcurrentMap, ConcurrentSkipListMap, ConcurrentHashMap}
import overlock.atomicmap.{OneShotThunk, AtomicMap}
import collection.mutable
import org.cliffc.high_scale_lib.{NonBlockingHashMap, NonBlockingHashMapi}

class AtomicMapi[A,B] extends mutable.Map[A,B] {
  val under : NonBlockingHashMap[A,Any] = new NonBlockingHashMap[A,Any]()

  override def empty = new AtomicMapi[A,B]

  override def getOrElseUpdate(key : A, op : => B) : B = {
    val t = new OneShotThunk(op)
    under.putIfAbsent(key, t) match {
      case null =>
        try {
          val ve = t.value
          under.replace(key, t, ve)
          //regardless of whether or not the replace worked, we must
          //return ve to remain within the bounds of the getOrElseUpdate
          // contract. If a concurrent update happened, this thread
          // ought not to see it until after returning from this call.
          ve
        } catch {
          case ex : Exception =>
            under.remove(key, t)
            throw ex
        }
      case OneShotThunk(v : B) => v
      case v : B => v
    }
  }

	def getOrElseUpdatei(key : A , op : => B ) : B = getOrElseUpdate(key,op)
//	def getOrElseUpdatei(key : Int, op : => B) : B = {
//	    val t = new OneShotThunk(op)
//	    under.putIfAbsenti(key, t) match {
//	      case null =>
//	        try {
//	          val ve = t.value
//	          under.replacei(key, t, ve)
//	          //regardless of whether or not the replace worked, we must
//	          //return ve to remain within the bounds of the getOrElseUpdate
//	          // contract. If a concurrent update happened, this thread
//	          // ought not to see it until after returning from this call.
//	          ve
//	        } catch {
//	          case ex : Exception =>
//	            under.remove(key, t)
//	            throw ex
//	        }
//	      case OneShotThunk(v : B) => v
//	      case v : B => v
//	    }
//	  }

	def getOrElsei ( key : A , defaultValue : B ) : B = { getOrElse(key,defaultValue) }

	def getOrElse ( key : A , defaultValue : B ) : B = {
		val r = under.get(key)
		r match {
			case null => defaultValue
			case OneShotThunk(someValue : B) => someValue
			case _ => r.asInstanceOf[B]
		}
	}
//	def getOrElsei ( key : Int , defaultValue : B ) : B = {
//		val r = under.geti(key)
//		if ( r == null ) { defaultValue }
//		else { r.asInstanceOf[B] }
//	}

  def replace(key : A, value : B) : Option[B] = {
    Option(under.replace(key,value)).map {
      case b : B => b
      case OneShotThunk(v : B) => v
    }
  }

  /**
   * This works because ConcurrentMap defines it as .equals
   */
  def replace(key : A, oldVal : B, newVal : B) : Boolean = {
    val oldThunk = new OneShotThunk(oldVal)
    under.replace(key, oldThunk, newVal)
  }

  def remove(key : A, value : B) : Boolean = {
    val thunk = new OneShotThunk(value)
    under.remove(key, thunk)
  }

  def putIfAbsent(key : A, value : B) : Option[B] = {
    Option(under.putIfAbsent(key,value)).map {
      case b : B => b
      case OneShotThunk(v : B) => v
    }
  }

  def -=(key : A) : this.type = {
    under.remove(key)
    this
  }

  def +=(kv : (A,B)) : this.type = {
    val (key,value) = kv
    under.put(key,value)
    this
  }

  def get(key : A) : Option[B] = {
    Option(under.get(key)) match {
      case None => None
      case Some(OneShotThunk(v : B)) => Some(v)
      case Some(v : B) => Some(v)
    }
  }

  def iterator : Iterator[(A,B)] = {
    new Iterator[(A,B)] {
      val iter = under.entrySet.iterator

      def next : (A,B) = {
        val entry = iter.next
        val key = entry.getKey
        entry.getValue match {
          case t : OneShotThunk[_] => (key,t.value.asInstanceOf[B])
          case v => (key,v.asInstanceOf[B])
        }
      }

      def hasNext : Boolean = {
        iter.hasNext
      }

      def remove {
        iter.remove
      }
    }
  }
}