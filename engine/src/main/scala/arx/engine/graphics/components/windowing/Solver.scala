package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.engine.control.components.windowing.Widget
import arx.graphics.VBO
import org.pybee.cassowary.Constraint
import org.pybee.cassowary.Expression
import org.pybee.cassowary.SimplexSolver
import org.pybee.cassowary.StayConstraint
import org.pybee.cassowary.Strength
import org.pybee.cassowary.Variable

import scala.collection.mutable
import scalaxy.loops._

class Solver {
	import Solver._

	case class DynamicExpression(expr : Expression, func : () => Int, name : String)

	val solver = new SimplexSolver

	val variables = new mutable.HashMap[String, Variable]()
	val constraints = new mutable.HashMap[String, ArxConstraint]
	val expressions = new mutable.HashMap[String, Expression]
	var dynamicExpressions = List[DynamicExpression]()

	var stays = new mutable.HashMap[String, StayConstraint]()

	def solve(force : Boolean): Unit = {
		if (force) {
			val field = solver.getClass.getDeclaredField("_fNeedsSolving")
			field.setAccessible(true)
			field.set(solver, true)
		}
		solver.solve()
	}

	def stayVariableAt(variable : Variable, value : Double): Unit = {
		if (variable.value() =~= value && stays.contains(variable.name())) {
			// do nothing, already exists with the right value
		} else {
			for (existing <- stays.get(variable.name())) {
				solver.removeConstraint(existing)
			}
			variable.set_value(value)
			val newConstraint = new StayConstraint(variable, Strength.REQUIRED, 100.0)
			solver.addConstraint(newConstraint)
			stays.put(variable.name(), newConstraint)
		}
	}
	def removeStay(variable : Variable): Unit = {
		for (existing <- stays.get(variable.name())) {
			solver.removeConstraint(existing)
			stays.remove(variable.name())
		}
	}

	def useConstraint(name: String, constraint: ArxConstraint): Unit = {
		constraints.get(name) match {
			case Some(c) =>
				solver.removeConstraint(c)
				constraints.remove(name)
			case _ => // do nothing
		}
		constraints.put(name, constraint)
		solver.addConstraint(constraint)
	}

	def vv(widget: Widget, suffix: String) = {
		val nomen = s"${widget.identifier}-$suffix"
		variables.getOrElseUpdate(nomen, {
			val v = new Variable(nomen)
			solver.addVar(v)
			v
		})
	}

	def ex(widget: Widget, suffix: String, f: () => Int) = {
		val name = s"${widget.identifier}-$suffix-dynvar"
		expressions.getOrElseUpdate(name, {
			//			val variable = new Variable(name)
			//			dynamicExpressions ::= (variable, f)
			//			solver.addStay(variable, Strength.WEAK)
			//			//			solver.addConstraint()
			//			//			solver.addEditVar(expr)
			//			variable
			val value = f() match {
				case 0.0f => 0.000001f
				case o => o
			}
			val expr = new Expression(value)
			dynamicExpressions ::= DynamicExpression(expr, f, name)

			expr
		})
	}

	def updateDynamicExpressions(): Boolean = {
		dynamicExpressions.map {
			case DynamicExpression(expr, func, name) =>
				val newValue = func()
				if (expr.constant().round != newValue) {
					Noto.info(s"Setting constant $name from ${expr.constant().round} to ${func()}")
					expr.set_constant(newValue)
					//
					//					solver.addEditVar(variable)
					//					solver.beginEdit()
					//					solver.suggestValue(variable, newValue)
					////					expr.change_value(newValue)
					//					solver.endEdit()
					//					println("\tExpr now is : " + variable)
					true
				} else {
					false
				}
		}.exists(identity) // apply all, then check if any were changed
	}


	def allConstraintDescriptions = constraints.values.map(c => c.description)
}

object Solver {
	implicit def toExprVar(v: Variable) = new Expression(v)

	implicit def toExprFloat(f: Float) = new Expression(f)

	implicit def toExprInt(i: Int) = new Expression(i)
}

class ArxConstraint(variable : Variable, op : Constraint.Operator, e : Expression, strength : Strength) extends Constraint(variable,op,e,strength) {
	def description : String = variable.name() + " " + op + " " + e
}

object Constraints {
	import Solver._

	def keepEqual(v: Variable, e: Expression, strength : Strength = Strength.STRONG) =
		new ArxConstraint(v, Constraint.Operator.EQ, e, strength)
	def keepLessBy(v: Variable, other: Expression, f: Float) =
		new ArxConstraint(v, Constraint.Operator.EQ, Expression.minus(other, f), Strength.STRONG)
	def keepMoreBy(v: Variable, other: Variable, f: Float) =
		new ArxConstraint(v, Constraint.Operator.EQ, Expression.plus(new Expression(other), new Expression(f)), Strength.STRONG)
	def keepTimesBy(v: Variable, other: Variable, f: Float) =
		new ArxConstraint(v, Constraint.Operator.EQ, Expression.times(new Expression(other), new Expression(f)), Strength.STRONG)
	def keepDivideBy(v: Variable, other: Variable, f: Float) =
		new ArxConstraint(v, Constraint.Operator.EQ, Expression.divide(new Expression(other), new Expression(f)), Strength.STRONG)
}