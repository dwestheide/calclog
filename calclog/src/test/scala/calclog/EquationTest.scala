package calclog

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.AnyOperators

object EquationTest extends SimpleTestSuite with Checkers {

  import calclog.Implicits._
  import ToStringFormat.Implicits._

  test("Equation for simple infix op") {
    check2((x: Int, y: Int) => {
      val xVar = x ~ "x"
      val yVar = y ~ "y"
      val e = xVar + yVar
      Equation.equation(e) ?= s"x + y = ${x + y}"
    })
  }

  test("Equation for simple function") {
    implicit val doubleFormat: Format[Double] = ToStringFormat.toStringFormat
    check1((x: Int) => {
      val xValue = math.pow(x.toDouble, 2)
      val xVar = math.pow(x.toDouble, 2) ~ "x"
      val e = xVar.sqrt
      Equation.equation(e) ?= s"sqrt(x) = ${math.sqrt(xValue)}"
    })
  }

  test("Nested expression uses parentheses") {
    check2((x: Int, y: Int) => {
      val xVar = x ~ "x"
      val yVar = y ~ "y"
      val e = xVar + yVar * yVar
      Equation.equation(e) ?= s"x + (y * y) = ${x + y * y}"
    })
  }


}
