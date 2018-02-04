package calclog

import calclog.Calculation.{Binding, Expression, Variable}
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.{BooleanOperators, AnyOperators}

object CalculationTest extends SimpleTestSuite with Checkers {

  import Implicits._
  import ValueFormatter.Implicits._
  import CalculationFormatter.syntax._

  test("extractValue of Variable") {
    check2((name: String, value: Int) =>
      (value > 0 && value < Int.MaxValue / 2) ==> {
        Variable(name, value).extractValue ?= Evaluated.success(value)
    })
  }

  test("extractValue of Expression") {
    check2((name: String, value: Int) =>
      (value > 0 && value < Int.MaxValue / 2) ==> {
        val v  = Variable(name, value)
        val op = Op.OneArgFunction[Int, Int](v, _ => Evaluated.success(value), "foo")
        Expression(op).extractValue ?= Evaluated.success(value)
    })
  }

  test("extractValue of Binding") {
    check3((varName: String, bindingName: String, value: Int) =>
      (value > 0 && value < Int.MaxValue / 2) ==> {
        val v  = Variable(varName, value)
        val op = Op.OneArgFunction[Int, Int](v, _ => Evaluated.success(value), "foo")
        val b  = Binding(bindingName, Expression(op))
        b.extractValue ?= Evaluated.success(value)
    })
  }

  test("calculationDefaultFormat for simple variable") {
    val x = 42 ~ "x"
    val formatted = "x = 42"
    assertEquals(x.run.description.format, formatted)
  }

  test("calculationDefaultFormat for simple expression") {
    val x = 42 ~ "x"
    val y = 23 ~ "y"
    val result = (x + y).run.description.format
    println(result)
  }


}
