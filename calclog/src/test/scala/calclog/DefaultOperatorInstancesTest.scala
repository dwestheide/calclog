package calclog

import java.time.Duration

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.{BooleanOperators, AnyOperators}

object DefaultOperatorInstancesTest extends SimpleTestSuite with Checkers with Arbitraries with DefaultOperatorInstances {

  test("BigDecimal addition") {
    check2((x: BigDecimal, y: BigDecimal) =>
      Plus[BigDecimal].apply(x, y) ?= Evaluated.success(x + y)
    )
  }

  test("Int addition") {
    check2((x: Int, y: Int) =>
      Plus[Int].apply(x, y) ?= Evaluated.success(x + y)
    )
  }

  test("Long addition") {
    check2((x: Long, y: Long) =>
      Plus[Long].apply(x, y) ?= Evaluated.success(x + y)
    )
  }

  test("Float addition") {
    check2((x: Float, y: Float) =>
      Plus[Float].apply(x, y) ?= Evaluated.success(x + y)
    )
  }

  test("Double addition") {
    check2((x: Double, y: Double) =>
      Plus[Double].apply(x, y) ?= Evaluated.success(x + y)
    )
  }

  test("Numeric subtraction") {
    check2((x: BigDecimal, y: BigDecimal) =>
      Minus[BigDecimal].apply(x, y) ?= Evaluated.success(x - y)
    )
  }

  test("Numeric multiplication") {
    check2((x: BigDecimal, y: BigDecimal) =>
      Times[BigDecimal, BigDecimal, BigDecimal].apply(x, y) ?= Evaluated.success(x * y)
    )
  }

  test("Fractional division") {
    check2((x: Double, y: Double) =>
      if (y != 0d)
        Divide[Double, Double, Double].apply(x, y) ?= Evaluated.success(x / y)
      else
        Divide[Double, Double, Double].apply(x, y) ?= Evaluated.failed("Division by zero")
    )
  }

  test("Duration addition") {
    check2((x: Duration, y: Duration) =>
      Plus[Duration].apply(x, y) ?= Evaluated.success(x.plus(y))
    )
  }

  test("Duration subtraction") {
    check2((x: Duration, y: Duration) =>
      Minus[Duration].apply(x, y) ?= Evaluated.success(x.minus(y))
    )
  }

  test("Duration multiplied by double") {
    check2((x: Duration, y: Double) =>
      Times[Duration, Double, Duration].apply(x, y) ?= Evaluated.success(x.multipliedBy(y.toLong))
    )
  }

  test("Duration division by double") {
    check2((x: Duration, y: Double) => (y.abs > 0.01) ==> {
      Divide[Duration, Double, Duration].apply(x, y) ?= Evaluated.success(x.dividedBy(y.toLong))
    })
  }

  test("Duration division by zero") {
    check1((x: Duration) =>
      Divide[Duration, Double, Duration].apply(x, 0) ?= Evaluated.failed("Cannot divide by zero")
    )
  }




}
