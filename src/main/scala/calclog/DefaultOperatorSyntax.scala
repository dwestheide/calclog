package calclog

trait DefaultOperatorSyntax: 
  
  extension [A](x: Calculation[A])
    def +(y: Calculation[A])(using plus: Plus[A], format: ValueFormatter[A]): Calculation.Expression[A] = 
      Calculation.Expression(Op.Infix(x, y, plus, "+"))
    def -(y: Calculation[A])(using minus: Minus[A], format: ValueFormatter[A]): Calculation.Expression[A] =
      Calculation.Expression(Op.Infix(x, y, minus, "-"))
    def *[B, C](y: Calculation[B])(using times: Times[A, B, C], format: ValueFormatter[C]): Calculation.Expression[C] =
      Calculation.Expression(Op.Infix(x, y, times, "*"))
    def /[B, C](y: Calculation[B])(using divide: Divide[A, B, C], format: ValueFormatter[C]): Calculation.Expression[C] =
      Calculation.Expression(Op.Infix(x, y, divide, "/"))
    def unary_-(using unaryMinus: UnaryMinus[A], format: ValueFormatter[A]) =
      Calculation.Expression(Op.Unary(x, unaryMinus, "-"))

  extension [A](x: A)
    def +(y: Calculation[A])(using plus: Plus[A], format: ValueFormatter[A]): Calculation.Expression[A] = 
      Calculation.Expression(Op.Infix(Calculation.Expression(Op.Literal(x, format)), y, plus, "+"))
    def -(y: Calculation[A])(using minus: Minus[A], format: ValueFormatter[A]): Calculation.Expression[A] =
      Calculation.Expression(Op.Infix(Calculation.Expression(Op.Literal(x, format)), y, minus, "-"))
    def *[B, C](y: Calculation[B])(using times: Times[A, B, C], formatA: ValueFormatter[A], formatC: ValueFormatter[C]): Calculation.Expression[C] =
      Calculation.Expression(Op.Infix(Calculation.Expression(Op.Literal(x, formatA)), y, times, "*"))
    def /[B, C](y: Calculation[B])(using divide: Divide[A, B, C], formatA: ValueFormatter[A], formatC: ValueFormatter[C]): Calculation.Expression[C] =
      Calculation.Expression(Op.Infix(Calculation.Expression(Op.Literal(x, formatA)), y, divide, "/"))
    def unary_-(using unaryMinus: UnaryMinus[A], format: ValueFormatter[A]) =
      Calculation.Expression(Op.Unary(Calculation.Expression(Op.Literal(x, format)), unaryMinus, "-"))

  def sqrt[A](x: Calculation[A])(using squareRoot: SquareRoot[A], format: ValueFormatter[A]): Calculation.Expression[A] =
    Calculation.Expression(Op.OneArgFunction(x, squareRoot, "sqrt"))

end DefaultOperatorSyntax

object DefaultOperatorSyntax extends DefaultOperatorSyntax
