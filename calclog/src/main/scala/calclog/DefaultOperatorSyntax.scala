package calclog

import calclog.Calculation.Expression

trait DefaultOperatorSyntax {

  implicit class AdditionOps[A: Plus: Format](x: Calculation[A]) {
    def +(y: Calculation[A]): Expression[A] = Expression(Op.Infix(x, y, Plus[A].apply, "+"))
  }

  implicit class SubtractionOps[A: Minus: Format](x: Calculation[A]) {
    def -(y: Calculation[A]): Expression[A] = Expression(Op.Infix(x, y, Minus[A].apply, "-"))
  }

  implicit class MultiplicationOps[A, B, C: Format](x: Calculation[A])(implicit times: Times[A, B, C]) {
    def *(y: Calculation[B]): Expression[C] = Expression(Op.Infix(x, y, times.apply, "*"))
  }

  implicit class DivisionOps[A, B, C: Format](x: Calculation[A])(implicit divide: Divide[A, B, C]) {
    def /(y: Calculation[B]): Expression[C] = Expression(Op.Infix(x, y, divide.apply, "/"))
  }

  implicit class SquareRootOps[A: SquareRoot: Format](x: Calculation[A]) {
    def sqrt: Expression[A] = Expression(Op.OneArgFunction(x, SquareRoot[A].apply, "sqrt"))
  }

}
