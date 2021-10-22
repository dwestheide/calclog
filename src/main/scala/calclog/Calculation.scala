package calclog

sealed trait CalculationDescription:
  def showValue: String
  def inputs: Seq[CalculationDescription]
  final def format(using formatter: CalculationDescriptionFormatter): String =
    formatter.format(this)

enum Calculation[A] extends CalculationDescription:
  case Variable(name: String, value: A, valueFormatter: ValueFormatter[A])
  case Expression(op: Op[A], valueFormatter: ValueFormatter[A])
  case Binding(name: String, expression: Expression[A], valueFormatter: ValueFormatter[A])

  def valueFormatter: ValueFormatter[A]
  
  def extractValue: Evaluated[A] = this match
    case Variable(_, value, _) => Evaluated.success(value)
    case Expression(op, _) => op.evaluated
    case Binding(name, expression, _) => expression
      .extractValue
      .left
      .map(reason => s"Error in expression bound to name '$name': $reason")
  
  def inputs: Seq[CalculationDescription] = this match
    case Variable(_, _, _) => Seq.empty
    case Expression(op, _) => op.description.inputs
    case Binding(_, expression, _) => expression.inputs

  final def showValue: String = extractValue.fold(identity, valueFormatter.formatValue)
  
  final def run: DescribedCalculation[A] = DescribedCalculation(extractValue, this)

end Calculation