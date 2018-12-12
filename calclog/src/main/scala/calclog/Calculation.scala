package calclog

import scala.collection.immutable.Seq

sealed trait CalculationDescription {
  def showValue: String
  def inputs: Seq[CalculationDescription]
  final def format(implicit formatter: CalculationDescriptionFormatter): String =
    formatter.format(this)
}

final case class CalculationSummary(name: String, value: String)

sealed trait CalcNode extends Product with Serializable
final case class DescriptionNode(indentLevel: Int, description: CalculationDescription) extends CalcNode
final case class ResultNode(result: String) extends CalcNode

object CalculationSummary {
  implicit val formatCalculationSummary: ValueFormatter[CalculationSummary] =
    summary => s"${summary.name} = ${summary.value}"
}

sealed abstract class Calculation[A: ValueFormatter] extends CalculationDescription with Product with Serializable {
  import ValueFormatterSyntax._
  def extractValue: Evaluated[A]
  final def showValue: String = extractValue.fold(identity, _.formatValue)
  final def run: DescribedCalculation[A] = DescribedCalculation(extractValue, this)
}

object Calculation {

  final case class Variable[A: ValueFormatter](name: String, value: A) extends Calculation[A] {
    override val extractValue: Evaluated[A] = Evaluated.success(value)
    override val inputs: Seq[CalculationDescription] = Seq.empty
  }

  final case class Expression[A: ValueFormatter](op: Op[A]) extends Calculation[A] {
    override val extractValue: Evaluated[A] = op.evaluated
    override val inputs: Seq[CalculationDescription] = op.inputs
  }

  final case class Binding[A: ValueFormatter](name: String, expression: Expression[A]) extends Calculation[A] {

    override val extractValue: Evaluated[A] = expression
      .extractValue
      .left
      .map(reason => s"Error in expression bound to name '$name': $reason")

    override val inputs: Seq[CalculationDescription] = expression.inputs
    
  }

}

