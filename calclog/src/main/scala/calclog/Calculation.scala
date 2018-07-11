package calclog

import calclog.Calculation.{Binding, Expression, Variable}

import scala.annotation.tailrec
import scala.collection.immutable.Seq

sealed trait CalculationDescription {
  def showValue: String
  def inputs: Seq[CalculationDescription]
}

final case class CalculationSummary(name: String, value: String)

sealed trait CalcNode extends Product with Serializable
final case class DescriptionNode(indentLevel: Int, description: CalculationDescription) extends CalcNode
final case class ResultNode(result: String) extends CalcNode

object CalculationSummary {
  implicit val formatCalculationSummary: CalculationFormatter[CalculationSummary] =
    summary => s"${summary.name} = ${summary.value}"
}

object CalculationDescription {
  implicit val calculationDefaultFormat: CalculationFormatter[CalculationDescription] = calculation => {
    import CalculationFormatter.syntax._
    val indent = "  "

    @tailrec
    def iter(acc: Seq[String],
             calculations: Seq[CalcNode]): Seq[String] = {
      calculations match {
        case Seq() => acc
        case head +: tail =>
          head match {
            case DescriptionNode(indentLevel, v @ Variable(_, _)) =>
              val summary =  indent * indentLevel + s"${CalculationSummary(v.name, v.showValue).format}"
              val newAcc =
                if (tail.exists { case DescriptionNode(_, d) => d == v; case _ => false }) acc
                else acc :+ summary
              iter(newAcc, tail)
            case DescriptionNode(indentLevel, b @ Binding(_, _)) =>
              val summary = indent * indentLevel + s"Calculating ${b.name}"
              val result = indent * indentLevel + s"Got ${b.name}: ${Equation.equation(b.expression)}"
              iter(acc :+ summary, b.inputs.map(desc => DescriptionNode(indentLevel + 1, desc)) ++
                Seq(ResultNode(result)) ++ tail)
            case DescriptionNode(indentLevel, e @ Expression(_)) =>
              if (indentLevel > 0)
                iter(acc, e.inputs.map(desc => DescriptionNode(indentLevel, desc)) ++ tail)
              else {
                val summary = indent * indentLevel + s"Calculating..."
                val result = indent * indentLevel + s"Got: ${Equation.equation(e)}"
                iter(acc :+ summary, e.inputs.map(desc => DescriptionNode(indentLevel + 1, desc)) ++
                  Seq(ResultNode(result)) ++ tail)
              }
            case ResultNode(result) =>
              iter(acc :+ result, tail)
          }
      }
    }
    iter(Seq.empty, Seq(calculation).map(DescriptionNode(0, _))).mkString("\n")
  }

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

