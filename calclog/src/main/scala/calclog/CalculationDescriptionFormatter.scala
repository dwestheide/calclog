package calclog
import calclog.Calculation.{Binding, Expression, Variable}

import scala.annotation.tailrec
import scala.collection.immutable.Seq

trait CalculationDescriptionFormatter {
  def format(calculationDescription: CalculationDescription): String
}

object CalculationDescriptionFormatter {
  object Implicits {
    implicit val default: CalculationDescriptionFormatter = calculation => {
      import ValueFormatterSyntax._
      val indent = "  "

      @tailrec
      def iter(acc: Seq[String],
               calculations: Seq[CalcNode]): Seq[String] = {
        calculations match {
          case Seq() => acc
          case head +: tail =>
            head match {
              case DescriptionNode(indentLevel, v @ Variable(_, _)) =>
                val summary =  indent * indentLevel + s"${CalculationSummary(v.name, v.showValue).formatValue}"
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
}