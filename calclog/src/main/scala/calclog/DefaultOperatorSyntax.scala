package calclog

import calclog.Calculation.Expression

trait DefaultOperatorSyntax { self =>

  implicit class AdditionOps[A](x: Calculation[A]) {
    def +(y: Calculation[A])(implicit plus: Plus[A], format: ValueFormatter[A]): Expression[A] =
      Expression(Op.Infix(x, y, Plus[A].apply, "+"))
  }

  implicit class SubtractionOps[A](x: Calculation[A]) {
    def -(y: Calculation[A])(implicit minus: Minus[A], format: ValueFormatter[A]): Expression[A] =
      Expression(Op.Infix(x, y, Minus[A].apply, "-"))
  }

  implicit class MultiplicationOps[A](x: Calculation[A]) {
    def *[B, C](y: Calculation[B])(implicit times: Times[A, B, C],
                                   format: ValueFormatter[C]): Expression[C] =
      Expression(Op.Infix(x, y, times.apply, "*"))
  }

  implicit class DivisionOps[A](x: Calculation[A]) {
    def /[B, C](y: Calculation[B])(implicit divide: Divide[A, B, C],
                                   format: ValueFormatter[C]): Expression[C] =
      Expression(Op.Infix(x, y, divide.apply, "/"))
  }

  implicit class SquareRootOps[A](x: Calculation[A]) {
    def sqrt(implicit squareRoot: SquareRoot[A], format: ValueFormatter[A]): Expression[A] =
      self.sqrt(x)
  }

  def sqrt[A](x: Calculation[A])(implicit squareRoot: SquareRoot[A],
                                 format: ValueFormatter[A]): Expression[A] =
    Expression(Op.OneArgFunction(x, SquareRoot[A].apply, "sqrt"))

  implicit class UnaryMinusOps[A](x: Calculation[A]) {
    def unary_-(implicit unaryMinus: UnaryMinus[A], format: ValueFormatter[A]) =
      Expression(Op.Unary(x, unaryMinus.apply, "-"))
  }

}

object DefaultOperatorSyntax extends DefaultOperatorSyntax
