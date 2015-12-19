package arx.core.function

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/23/13
 * Time: 1:34 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._

import arx.application.Noto
import scala.collection.immutable.Queue
import scala.collection.mutable
import arx.core.{MutableModdable, Moddable}

trait TArithmeticExpression extends Moddable[Float] {
	def symbolicValues_= ( sym : Map[String,Moddable[Float]] )
	def symbolicValues : Map[String,Moddable[Float]]
	def withSymbolicValues ( sym : Map[String,Moddable[Float]] ) = {
		symbolicValues = sym
		this
	}

	def reify : TArithmeticExpression

	def resolve() = evaluate()
	def baseValue() = evaluate()
	def evaluate () : Float
	def evaluate ( symbolMap : Map[String,Moddable[Float]] ) : Float

	def evaluateMin ( symbolMap : Map[String,Moddable[Float]] ) : Float
	def evaluateMax ( symbolMap : Map[String,Moddable[Float]] ) : Float
}

class ArithmeticExpression extends TArithmeticExpression {
	var stringRepresentation : String = ""
	var operations = List[Any]()
	var symbolicValues = Map[String,Moddable[Float]]()


	def reify = {
		if ( operations.exists { case v : ArithmeticExpression.Value => ! v.isConstant ; case _ => false } ) {
			this
		} else {
			new SimpleExpression(evaluate())
		}
	}

	def evaluate ( symbolMap : Map[String,Moddable[Float]], minimax : Int ) : Float = {
		val stack = mutable.Stack[Float]()
		operations.foreach {
			case op : ArithmeticExpression.Operator => {
				stack.push( op( stack.pop() , stack.pop() ) )
			}
			case value : ArithmeticExpression.Value => {
				minimax match {
					case -1 => stack.push( value.resolveMin( symbolicValues ) )
					case 0 => stack.push( value.resolve( symbolicValues ) )
					case 1 => stack.push( value.resolveMax( symbolicValues ) )
				}
			}
		}

		stack.top
	}

	def evaluate ( symbolMap : Map[String,Moddable[Float]] ) : Float = {
		evaluate(symbolMap,ArithmeticExpression.minMaxMode.get)
	}

	def evaluate () = {
		evaluate( symbolicValues , ArithmeticExpression.minMaxMode.get )
	}

	def evaluateMin(symbolMap: Map[String, Moddable[Float]]): Float = evaluate(symbolMap,-1)
	def evaluateMax(symbolMap: Map[String, Moddable[Float]]): Float = evaluate(symbolMap,1)
}

class SimpleExpression ( value : Float ) extends TArithmeticExpression {
	def evaluate() = value
	def evaluate(symbolMap: Map[String, Moddable[Float]]) = value

	def symbolicValues_=(sym: Map[String, Moddable[Float]]) {}
	def symbolicValues = Map()


	def evaluateMin(symbolMap: Map[String, Moddable[Float]]): Float = value
	def evaluateMax(symbolMap: Map[String, Moddable[Float]]): Float = value

	def reify = this
}

object ArithmeticExpression {
	val minMaxMode = new ThreadLocal[Int] {
		override def initialValue(): Int = 0
	}

	def minBlock ( stmt : => Unit ) {
		val original = minMaxMode.get
		minMaxMode.set(-1)

		stmt

		minMaxMode.set(original)
	}
	def maxBlock ( stmt : => Unit ) {
		val original = minMaxMode.get
		minMaxMode.set(1)

		stmt

		minMaxMode.set(original)
	}


	val operatorMap = Map("+" -> Add,"-" -> Sub,"*" -> Mul,"/" -> Div)
	val allDelimiters = " \\+\\-/\\*\\(\\)"
	val splitter = f"((?<=[$allDelimiters])|(?=[$allDelimiters]))"

	def fromString ( str : String ) : ArithmeticExpression = {
		val tokens = str.split(splitter)

		var stack = List[Operator]()
		var output = Queue[Any]()

		var infix = false

		var t = 0; while ( t < tokens.length ) {
			val token = tokens(t)
			token match {
				case "" => //ignore
				case " " => //ignore
				case "(" => {
					stack ::= OpenParens
				}
				case ")" => {
					while ( stack.nonEmpty && stack.head != OpenParens ) {
						output :+= stack.head
						stack = stack.tail
					}
					stack = stack.tail
					infix = true
				}
				case other => {
					if ( infix && operatorMap.contains(token) ) {
						val operator = operatorMap(token)
						while ( stack.nonEmpty && stack.head.precedence >= operator.precedence ) {
							output :+= stack.head
							stack = stack.tail
						}
						stack ::= operator
						infix = false
					} else {
						val newToken = if ( ! infix && operatorMap.contains(token) ) {
							t += 1
							token + tokens(t)
						} else {
							token
						}

						if ( newToken(0).isDigit && newToken.contains( "d" ) ) {
							val subnewTokens = newToken.split("d")
							output :+= RandomValue(subnewTokens(0).toInt,subnewTokens(1).toInt)
						} else if ( newToken(0) == '.' || newToken(0).isDigit || newToken(0) == '-' || newToken(0) == '+' ) {
							output :+= Constant(newToken.toFloat)
						} else {
							output :+= SymbolicValue(newToken)
						}
						infix = true
					}
				}
			}
			t += 1
		}
		while ( stack.nonEmpty ) {
			output :+= stack.head
			stack = stack.tail
		}

		val expr = new ArithmeticExpression
		expr.operations = output.toList
		expr.stringRepresentation = str
		expr
	}

	def fromValue ( i : Int ) = {
		new SimpleExpression(i)
	}

	def fromValue ( f : Float ) = {
		new SimpleExpression(f)
	}

	abstract class Operator(val precedence : Int) {
		def apply ( x : Float , y : Float ) : Float
	}
	object Add extends Operator(0) { def apply ( y:Float,x:Float ) = x + y }
	object Sub extends Operator(0) { def apply ( y:Float,x:Float ) = x - y }
	object Mul extends Operator(1) { def apply ( y:Float,x:Float ) = x * y }
	object Div extends Operator(1) { def apply ( y:Float,x:Float ) = x / y }
	object OpenParens extends Operator(-1000) { def apply(x:Float,y:Float) = Float.NaN }

	abstract class Value {
		def resolve ( symbolMap : Map[String,Moddable[Float]] ) : Float
		def resolveMin ( symbolMap : Map[String,Moddable[Float]] ) : Float = resolve(symbolMap)
		def resolveMax ( symbolMap : Map[String,Moddable[Float]] ) : Float = resolve(symbolMap)
		def isConstant = true
	}
	case class Constant ( f : Float ) extends Value {
		def resolve ( symbolMap : Map[String,Moddable[Float]] ) = f
	}
	case class SymbolicValue ( sym : String ) extends Value {
		def resolve(symbolMap: Map[String, Moddable[Float]]) = { symbolMap(sym).resolve() }
	}
	case class RandomValue ( numDice : Int , diceSize : Int ) extends Value {
		def resolve(symbolMap: Map[String, Moddable[Float]]) = {
			var sum = 0
			var i = 0; while ( i < numDice ) {
				sum += rand(1,diceSize)
			i += 1}
			sum
		}

		override def resolveMin(symbolMap: Map[String, Moddable[Float]]): Float = numDice
		override def resolveMax(symbolMap: Map[String, Moddable[Float]]): Float = numDice * diceSize

		override def isConstant = false
	}
}

object TArithmeticExpression {
	implicit class MutableModdableArithmeticExpression ( expr : TArithmeticExpression ) extends MutableModdable(0.0f) {
		intern = expr
	}
}
