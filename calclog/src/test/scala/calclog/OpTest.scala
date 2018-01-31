package calclog

import minitest.SimpleTestSuite
import minitest.laws.Checkers
import org.scalacheck.Prop._
import ToStringFormat.Implicits._

object OpTest extends SimpleTestSuite with Checkers {

  test("Infix op applies f to left and right value") {
    check4((x: Int, y: Int, f: (Int, Int) => Evaluated[Boolean], s: String) => {
      val left = Calculation.Variable("left", x)
      val right = Calculation.Variable("right", y)
      val infixOp: Op.Infix[Int, Int, Boolean] = Op.Infix(left, right, f, s)
      infixOp.evaluated ?= f(x, y)
    })
  }

  test("Infix op short-circuits if left side is a failure") {
    check3((y: Int, f: (Int, Int) => Evaluated[Boolean], s: String) => {
      val left = failedExpression[Int]("failure")
      val right = Calculation.Variable("right", y)
      val infixOp: Op.Infix[Int, Int, Boolean] = Op.Infix(left, right, f, s)
      infixOp.evaluated ?= Evaluated.failed("failure")
    })
  }

  test("Infix op short-circuits if right side is a failure") {
    check3((x: Int, f: (Int, Int) => Evaluated[Boolean], s: String) => {
      val left = Calculation.Variable("left", x)
      val right = failedExpression[Int]("failure")
      val infixOp: Op.Infix[Int, Int, Boolean] = Op.Infix(left, right, f, s)
      infixOp.evaluated ?= Evaluated.failed("failure")
    })
  }

  test("OneArgFunction applies f to the argument") {
    check3((x: Int, f: Int => Evaluated[Boolean], s: String) => {
      val func = Op.OneArgFunction(Calculation.Variable("x", x), f, s)
      func.evaluated ?= f(x)
    })
  }

  test("OneArgFunction short-circuits if argument is a failure") {
    check2((f: Int => Evaluated[Boolean], s: String) => {
      val func = Op.OneArgFunction(failedExpression[Int]("failure"), f, s)
      func.evaluated ?= Evaluated.failed("failure")
    })
  }

  private def failedExpression[A: Format](reason: String): Calculation.Expression[A] =
    Calculation.Expression(Op.OneArgFunction(
      Calculation.Variable("foo", 0),
      (_: Int) => Evaluated.failed[A](reason),
      "failing"))


}
