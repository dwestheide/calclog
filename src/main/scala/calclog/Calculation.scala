package calclog

sealed trait CalculationDescription:
  def showValue: String
  def inputs: Seq[CalculationDescription]
  final def format(using formatter: CalculationDescriptionFormatter): String =
    formatter.format(this)

sealed abstract class Calculation[A: ValueFormatter] extends CalculationDescription with Product with Serializable:
  def extractValue: Evaluated[A]
  final def showValue: String = extractValue.fold(identity, _.formatValue)
  final def run: DescribedCalculation[A] = DescribedCalculation(extractValue, this)

end Calculation

object Calculation:
  final case class Variable[A: ValueFormatter](name: String, value: A) extends Calculation[A]:
    override val extractValue: Evaluated[A] = Evaluated.success(value)
    override val inputs: Seq[CalculationDescription] = Seq.empty

  final case class Expression[A: ValueFormatter](op: Op[A]) extends Calculation[A]:
    override val extractValue: Evaluated[A] = op.evaluated
    override val inputs: Seq[CalculationDescription] = op.description.inputs

  final case class Binding[A: ValueFormatter](name: String, expression: Expression[A]) extends Calculation[A]:

    override val extractValue: Evaluated[A] = expression
      .extractValue
      .left
      .map(reason => s"Error in expression bound to name '$name': $reason")

    override val inputs: Seq[CalculationDescription] = expression.inputs
