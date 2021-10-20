package calclog

enum Op[A]:
  case Infix[A, B, C](
    left: Calculation[A],
    right: Calculation[B],
    f: (A, B) => Evaluated[C],
    symbol: String) extends Op[C]

  case OneArgFunction[A, B](
    x: Calculation[A],
    f: A => Evaluated[B],
    name: String) extends Op[B]
  
  case TwoArgsFunction[A, B, C](
    x: Calculation[A],
    y: Calculation[B],
    f: (A, B) => Evaluated[C],
    name: String) extends Op[C]
  
  case Literal[A](value: A, valueFormatter: ValueFormatter[A]) extends Op[A]

  case Unary[A](
    operand: Calculation[A], 
    f: A => Evaluated[A], 
    symbol: String) extends Op[A]

  def evaluated: Evaluated[A] = this match
    case Infix(left, right, f, symbol) =>
      for
        a <- left.extractValue
        b <- right.extractValue
        c <- f(a, b)
      yield c
    case OneArgFunction(x, f, name) =>
      for
        a <- x.extractValue
        b <- f(a)
      yield b
    case TwoArgsFunction(x, y, f, name) =>
      for
        a <- x.extractValue
        b <- y.extractValue
        c <- f(a, b)
      yield c
    case Literal(value, _) => Evaluated.success(value)
    case Unary(operand, f, symbol) =>
      for
        a  <- operand.extractValue
        a2 <- f(a)
      yield a2
  
    def description: OpDescription = this match
      case Infix(left, right, _, symbol) => OpDescription.Infix(left, right, symbol)
      case Literal(value, formatter) => OpDescription.Literal(formatter.format(value))
      case Unary(operand, _, symbol) => OpDescription.Unary(symbol, operand)
      case OneArgFunction(x, _, name) => OpDescription.Func(name, Seq(x))
      case TwoArgsFunction(x, y, _, name) => OpDescription.Func(name, Seq(x, y))
end Op

enum OpDescription(val inputs: Seq[CalculationDescription]):
  case Infix(
    left: CalculationDescription,
    right: CalculationDescription,
    symbol: String) extends OpDescription(Seq(left, right))
  case Func(name: String, arguments: Seq[CalculationDescription]) extends OpDescription(arguments)
  case Literal(valueString: String) extends OpDescription(Seq.empty)
  case Unary(symbol: String, operand: CalculationDescription) extends OpDescription(Seq(operand))