package calclog

import calclog.Calculation.Expression
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.AnyOperators

object EquationTest extends SimpleTestSuite with Checkers:

  import calclog.Defaults.{given, *}
  import ValueFormatter.toStringValueFormatter

  test("Equation for simple infix op") {
    check2((x: Int, y: Int) => {
      val xVar = x ~ "x"
      val yVar = y ~ "y"
      val e = xVar + yVar
      equation(e) ?= s"x + y = ${x + y}"
    })
  }

  test("Equation for simple function") {
    check1((x: Int) => {
      val xValue = math.pow(x.toDouble, 2)
      val xVar = math.pow(x.toDouble, 2) ~ "x"
      val e = sqrt(xVar)
      equation(e) ?= s"sqrt(x) = ${math.sqrt(xValue)}"
    })
  }

  test("Nested expression uses parentheses") {
    check2((x: Int, y: Int) => {
      val xVar = x ~ "x"
      val yVar = y ~ "y"
      val e = xVar + yVar * yVar
      equation(e) ?= s"x + (y * y) = ${x + y * y}"
    })
  }

  test("Equation with simple literal ops") {
    check1((x: Int) => {
      val literal = Expression(Op.Literal(x, toStringValueFormatter), toStringValueFormatter)
      equation(literal + literal) ?= s"$x + $x = ${x + x}"
    })
  }

  test("Equation with simple unary operator") {
    check1((x: Int) => {
      val xVar = x ~ "x"
      val e = new Expression(Op.Unary[Int](xVar, a => Evaluated.success(-a), "-"), toStringValueFormatter)
      equation(e) ?= s"-x = ${-x}"
    })
  }

end EquationTest