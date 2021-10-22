package calclog

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import calclog.ValueFormatter.toStringValueFormatter
import org.scalacheck.Prop._

object OpTest extends SimpleTestSuite with Checkers:

  import Defaults.given

  test("Infix op applies f to left and right value") {
    check4((x: Int, y: Int, f: (Int, Int) => Evaluated[Boolean], s: String) => {
      val left = Calculation.Variable("left", x, toStringValueFormatter)
      val right = Calculation.Variable("right", y, toStringValueFormatter)
      val infixOp: Op.Infix[Int, Int, Boolean] = Op.Infix(left, right, f, s)
      infixOp.evaluated ?= f(x, y)
    })
  }

  test("Infix op short-circuits if left side is a failure") {
    check3((y: Int, f: (Int, Int) => Evaluated[Boolean], s: String) => {
      val left = failedExpression[Int]("failure")
      val right = Calculation.Variable("right", y, toStringValueFormatter)
      val infixOp: Op.Infix[Int, Int, Boolean] = Op.Infix(left, right, f, s)
      infixOp.evaluated ?= Evaluated.failed("failure")
    })
  }

  test("Infix op short-circuits if right side is a failure") {
    check3((x: Int, f: (Int, Int) => Evaluated[Boolean], s: String) => {
      val left = Calculation.Variable("left", x, toStringValueFormatter)
      val right = failedExpression[Int]("failure")
      val infixOp: Op.Infix[Int, Int, Boolean] = Op.Infix(left, right, f, s)
      infixOp.evaluated ?= Evaluated.failed("failure")
    })
  }

  test("OneArgFunction applies f to the argument") {
    check3((x: Int, f: Int => Evaluated[Boolean], s: String) => {
      val func = Op.OneArgFunction(Calculation.Variable("x", x, toStringValueFormatter), f, s)
      func.evaluated ?= f(x)
    })
  }

  test("OneArgFunction short-circuits if argument is a failure") {
    check2((f: Int => Evaluated[Boolean], s: String) => {
      val func = Op.OneArgFunction(failedExpression[Int]("failure"), f, s)
      func.evaluated ?= Evaluated.failed("failure")
    })
  }

  test("Literal evaluates to passed in value") {
    check1((x: Int) => {
      val l = Op.Literal(x, ValueFormatter.toStringValueFormatter)
      l.evaluated ?= Evaluated.success(x)
    })
  }

  test("Unary applies f to the argument") {
    check3((x: Int, f: Int => Evaluated[Int], s: String) => {
      val func = Op.Unary(Calculation.Variable("x", x, toStringValueFormatter), f, s)
      func.evaluated ?= f(x)
    })
  }

  private def failedExpression[A](reason: String): Calculation.Expression[A] =
    Calculation.Expression(Op.OneArgFunction(
      Calculation.Variable("foo", 0, toStringValueFormatter),
      (_: Int) => Evaluated.failed[A](reason),
      "failing"), toStringValueFormatter)

end OpTest