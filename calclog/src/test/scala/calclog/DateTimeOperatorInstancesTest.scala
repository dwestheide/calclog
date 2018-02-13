package calclog

import java.time.Duration

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop.{AnyOperators, BooleanOperators}

object DateTimeOperatorInstancesTest extends SimpleTestSuite with Checkers with Arbitraries {

  import calclog.datetime._

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

  test("Duration division by int") {
    check2((x: Duration, y: Int) => y != 0 ==> {
      Divide[Duration, Int, Duration].apply(x, y) ?= Evaluated.success(x.dividedBy(y.toLong))
    })
  }

  test("Duration division by int zero") {
    check1((x: Duration) =>
      Divide[Duration, Int, Duration].apply(x, 0) ?= Evaluated.failed("Cannot divide by zero")
    )
  }

  test("Duration division by long") {
    check2((x: Duration, y: Long) => y != 0 ==> {
      Divide[Duration, Long, Duration].apply(x, y) ?= Evaluated.success(x.dividedBy(y))
    })
  }

  test("Duration division by long zero") {
    check1((x: Duration) =>
      Divide[Duration, Long, Duration].apply(x, 0) ?= Evaluated.failed("Cannot divide by zero")
    )
  }

}
