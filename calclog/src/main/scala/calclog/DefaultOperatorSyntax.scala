package calclog

import calclog.Calculation.Expression

trait DefaultOperatorSyntax {

  implicit class AdditionOps[A](x: Calculation[A])(implicit plus: Plus[A], format: Format[A]) {
    def +(y: Calculation[A]): Expression[A] = Expression(Op.Infix(x, y, Plus[A].apply, "+"))
  }

  implicit class SubtractionOps[A](x: Calculation[A])(implicit minus: Minus[A], format: Format[A]) {
    def -(y: Calculation[A]): Expression[A] = Expression(Op.Infix(x, y, Minus[A].apply, "-"))
  }

  implicit class MultiplicationOps[A, B, C](x: Calculation[A])(implicit times: Times[A, B, C], format: Format[C]) {
    def *(y: Calculation[B]): Expression[C] = Expression(Op.Infix(x, y, times.apply, "*"))
  }

  implicit class DivisionOps[A, B, C](x: Calculation[A])(implicit divide: Divide[A, B, C], format: Format[C]) {
    def /(y: Calculation[B]): Expression[C] = Expression(Op.Infix(x, y, divide.apply, "/"))
  }

  implicit class SquareRootOps[A](x: Calculation[A])(implicit squareRoot: SquareRoot[A], format: Format[A]) {
    def sqrt: Expression[A] = Expression(Op.OneArgFunction(x, SquareRoot[A].apply, "sqrt"))
  }

}
