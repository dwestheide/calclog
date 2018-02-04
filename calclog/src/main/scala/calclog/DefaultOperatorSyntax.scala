package calclog

import calclog.Calculation.Expression

trait DefaultOperatorSyntax {

  implicit class AdditionOps[A](x: Calculation[A])(implicit plus: Plus[A]) {
    def +(y: Calculation[A])(implicit format: ValueFormatter[A]): Expression[A] =
      Expression(Op.Infix(x, y, Plus[A].apply, "+"))
  }

  implicit class SubtractionOps[A](x: Calculation[A])(implicit minus: Minus[A]) {
    def -(y: Calculation[A])(implicit format: ValueFormatter[A]): Expression[A] =
      Expression(Op.Infix(x, y, Minus[A].apply, "-"))
  }

  implicit class MultiplicationOps[A, B, C](x: Calculation[A])(implicit times: Times[A, B, C]) {
    def *(y: Calculation[B])(implicit format: ValueFormatter[C]): Expression[C] =
      Expression(Op.Infix(x, y, times.apply, "*"))
  }

  implicit class DivisionOps[A, B, C](x: Calculation[A])(implicit divide: Divide[A, B, C]) {
    def /(y: Calculation[B])(implicit format: ValueFormatter[C]): Expression[C] =
      Expression(Op.Infix(x, y, divide.apply, "/"))
  }

  implicit class SquareRootOps[A](x: Calculation[A])(implicit squareRoot: SquareRoot[A]) {
    def sqrt(implicit format: ValueFormatter[A]): Expression[A] =
      Expression(Op.OneArgFunction(x, SquareRoot[A].apply, "sqrt"))
  }

}

object DefaultOperatorSyntax extends DefaultOperatorSyntax
