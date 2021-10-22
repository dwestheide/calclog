package calclog

import calclog.Calculation.{Binding, Expression, Variable}
import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.given
import calclog.CalculationTest

object CalculationTest extends SimpleTestSuite with Checkers:

  import Defaults.{given, *}
  import ValueFormatter.toStringValueFormatter
  import CalculationDescriptionFormatter.given

  test("extractValue of Variable") {
    check2((name: String, value: Int) =>
      (value > 0 && value < Int.MaxValue / 2) ==> {
        Variable(name, value, toStringValueFormatter).extractValue ?= Evaluated.success(value)
    })
  }

  test("extractValue of Expression") {
    check2((name: String, value: Int) =>
      (value > 0 && value < Int.MaxValue / 2) ==> {
        val v  = Variable(name, value, toStringValueFormatter)
        val op = Op.OneArgFunction[Int, Int](v, _ => Evaluated.success(value), "foo")
        Expression(op, toStringValueFormatter).extractValue ?= Evaluated.success(value)
    })
  }

  test("extractValue of Binding") {
    check3((varName: String, bindingName: String, value: Int) =>
      (value > 0 && value < Int.MaxValue / 2) ==> {
        val v  = Variable(varName, value, toStringValueFormatter)
        val op = Op.OneArgFunction[Int, Int](v, _ => Evaluated.success(value), "foo")
        val b  = Binding(bindingName, Expression(op, toStringValueFormatter), toStringValueFormatter)
        b.extractValue ?= Evaluated.success(value)
    })
  }

  test("calculationDefaultFormat for simple variable") {
    val x = 42 ~ "x"
    val formatted = "x = 42"
    assertEquals(x.run.description.format, formatted)
  }

  test("calculationDefaultFormat for simple expression") {
    check2((xValue: Int, yValue: Int) => {
      val x = xValue ~ "x"
      val y = yValue ~ "y"
      val expected =
        s"""Calculating...
          |  x = $xValue
          |  y = $yValue
          |Got: x + y = ${xValue + yValue}""".stripMargin
      (x + y).run.description.format ?= expected
    })
  }

  test("calculationDefaultFormat for nested expression") {
    check3((xValue: Int, yValue: Int, zValue: Int) => {
      val x = xValue ~ "x"
      val y = yValue ~ "y"
      val z = zValue ~ "z"
      val expected =
        s"""Calculating...
           |  x = $xValue
           |  y = $yValue
           |  z = $zValue
           |Got: x + (y * z) = ${xValue + yValue * zValue}""".stripMargin
      (x + y * z).run.description.format ?= expected
    })
  }

  test("calculationDefaultFormat for simple binding") {
    check2((xValue: Int, yValue: Int) => {
      val x = xValue ~ "x"
      val y = yValue ~ "y"
      val z = x + y <~ "z"
      val expected =
        s"""Calculating z
           |  x = $xValue
           |  y = $yValue
           |Got z: x + y = ${xValue + yValue}""".stripMargin
      z.run.description.format ?= expected
    })
  }

  test("calculationDefaultFormat for binding with nested expression") {
    check3((xValue: Int, yValue: Int, zValue: Int) => {
      val x = xValue ~ "x"
      val y = yValue ~ "y"
      val z = zValue ~ "z"
      val result = x + y * z <~ "result"
      val expected =
        s"""Calculating result
           |  x = $xValue
           |  y = $yValue
           |  z = $zValue
           |Got result: x + (y * z) = ${xValue + yValue * zValue}""".stripMargin
      result.run.description.format ?= expected
    })
  }

  test("calculationDefaultFormat for nested binding") {
    check3((xValue: Int, yValue: Int, zValue: Int) => {
      val x = xValue ~ "x"
      val y = yValue ~ "y"
      val z = zValue ~ "z"
      val xTimesY = x * y <~ "xTimesY"
      val xTimesZ = x * z <~ "xTimesZ"
      val intermediate1 = xTimesY + xTimesZ <~ "intermediate1"
      val intermediate2 = xTimesY * xTimesZ <~ "intermediate2"
      val result = intermediate1 + intermediate2 <~ "result"
      val xTimesYValue = xValue * yValue
      val xTimesZValue = xValue * zValue
      val intermediate1Value = xTimesYValue + xTimesZValue
      val intermediate2Value = xTimesYValue * xTimesZValue
      val resultValue = intermediate1Value + intermediate2Value
      val expected =
        s"""Calculating result
          |  Calculating intermediate1
          |    Calculating xTimesY
          |      x = $xValue
          |      y = $yValue
          |    Got xTimesY: x * y = $xTimesYValue
          |    Calculating xTimesZ
          |      x = $xValue
          |      z = $zValue
          |    Got xTimesZ: x * z = $xTimesZValue
          |  Got intermediate1: xTimesY + xTimesZ = $intermediate1Value
          |  Calculating intermediate2
          |    Calculating xTimesY
          |      x = $xValue
          |      y = $yValue
          |    Got xTimesY: x * y = $xTimesYValue
          |    Calculating xTimesZ
          |      x = $xValue
          |      z = $zValue
          |    Got xTimesZ: x * z = $xTimesZValue
          |  Got intermediate2: xTimesY * xTimesZ = $intermediate2Value
          |Got result: intermediate1 + intermediate2 = $resultValue""".stripMargin
      result.run.description.format ?= expected
    })
  }

end CalculationTest