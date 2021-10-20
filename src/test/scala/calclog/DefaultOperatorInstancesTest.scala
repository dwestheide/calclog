package calclog

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.AnyOperators

object DefaultOperatorInstancesTest extends SimpleTestSuite with Checkers:

  import Defaults.given

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

end DefaultOperatorInstancesTest