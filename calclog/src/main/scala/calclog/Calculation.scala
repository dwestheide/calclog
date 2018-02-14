package calclog

import calclog.Calculation.{Binding, Expression, Variable}

import scala.collection.immutable.Seq

sealed trait CalculationDescription {
  def showValue: String
  def inputs: Seq[CalculationDescription]
}

final case class CalculationSummary(name: String, value: String)

object CalculationSummary {
  implicit val formatCalculationSummary: CalculationFormatter[CalculationSummary] =
    summary => s"${summary.name} = ${summary.value}"
}

object CalculationDescription {
  implicit val calculationDefaultFormat: CalculationFormatter[CalculationDescription] = calculation => {
    import CalculationFormatter.syntax._
    val indent = "  "
    def iter(acc: Seq[String],
             indentLevel: Int,
             calculations: Seq[CalculationDescription]): Seq[String] = {
      calculations match {
        case Seq() => acc
        case head +: tail =>
          head match {
            case v @ Variable(_, _) =>
              val summary =  indent * indentLevel + s"${CalculationSummary(v.name, v.showValue).format}"
              val newAcc =
                if (tail.contains(v)) acc
                else acc :+ summary
              iter(newAcc, indentLevel, tail)
            case b @ Binding(_, _) =>
              val summary =  indent * indentLevel + s"Calculating ${b.name}"
              val newAcc = if (tail.contains(b)) acc else acc :+ summary
              val result = indent * indentLevel + s"Got ${b.name}: ${Equation.equation(b.expression)}"
              val withInputs = iter(newAcc, indentLevel + 1, b.inputs) :+ result
              iter(withInputs, indentLevel, tail)
            case e @ Expression(_) =>
              if (indentLevel > 0)
                iter(acc, indentLevel, e.inputs ++ tail)
              else {
                val summary = indent * indentLevel + s"Calculating..."
                val newAcc = acc :+ summary
                val result = indent * indentLevel + s"Got: ${Equation.equation(e)}"
                val withInputs = iter(newAcc, indentLevel + 1, e.inputs) :+ result
                iter(withInputs, indentLevel, tail)
              }
          }
      }
    }
    iter(Seq.empty, 0, Seq(calculation)).mkString("\n")
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

