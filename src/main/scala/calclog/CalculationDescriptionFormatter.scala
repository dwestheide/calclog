package calclog

trait CalculationDescriptionFormatter:
  def format(calculationDescription: CalculationDescription): String

object CalculationDescriptionFormatter:
  private[this] final case class CalculationSummary(name: String, value: String)
  private[this] object CalculationSummary:
    given ValueFormatter[CalculationSummary] = 
      summary => s"${summary.name} = ${summary.value}"

  private[this] enum CalcNode:
    case DescriptionNode(indentLevel: Int, description: CalculationDescription)
    case ResultNode(result: String)
  
  given default: CalculationDescriptionFormatter = calculation =>
      val indent = "  "

      @scala.annotation.tailrec
      def iter(acc: Seq[String], calculations: Seq[CalcNode]): Seq[String] =
        calculations match
          case Seq() => acc
          case head +: tail =>
            head match
              case CalcNode.DescriptionNode(indentLevel, v @ Calculation.Variable(_, _, _)) =>
                val summary =  indent * indentLevel + s"${CalculationSummary(v.name, v.showValue).formatValue}"
                val newAcc =
                  if tail.exists { case CalcNode.DescriptionNode(_, d) => d == v; case _ => false } then acc
                  else acc :+ summary
                iter(newAcc, tail)
              case CalcNode.DescriptionNode(indentLevel, b @ Calculation.Binding(_, _, _)) =>
                val summary = indent * indentLevel + s"Calculating ${b.name}"
                val result = indent * indentLevel + s"Got ${b.name}: ${equation(b.expression)}"
                iter(acc :+ summary, b.inputs.map(desc => CalcNode.DescriptionNode(indentLevel + 1, desc)) ++
                  Seq(CalcNode.ResultNode(result)) ++ tail)
              case CalcNode.DescriptionNode(indentLevel, e @ Calculation.Expression(_, _)) =>
                if indentLevel > 0 then
                  iter(acc, e.inputs.map(desc => CalcNode.DescriptionNode(indentLevel, desc)) ++ tail)
                else
                  val summary = indent * indentLevel + s"Calculating..."
                  val result = indent * indentLevel + s"Got: ${equation(e)}"
                  iter(acc :+ summary, e.inputs.map(desc => CalcNode.DescriptionNode(indentLevel + 1, desc)) ++
                    Seq(CalcNode.ResultNode(result)) ++ tail)
              case CalcNode.ResultNode(result) =>
                iter(acc :+ result, tail)
      end iter
      iter(Seq.empty, Seq(calculation).map(CalcNode.DescriptionNode(0, _))).mkString("\n")

  val flat: CalculationDescriptionFormatter = calculation =>
    val heading = calculation match
      case Calculation.Binding(name, _, _) =>
        s"Calculating $name..."
      case Calculation.Expression(_, _) =>
        "Calculating expression..."
      case Calculation.Variable(name, _, _) =>
        s"Defining variable $name"
      
      val result = calculation match
      case b @ Calculation.Binding(name, _, _) =>
        CalculationSummary(name, b.showValue).formatValue
      case e @ Calculation.Expression(_, _) =>
        s"Expression result = ${e.showValue}"
      case v @ Calculation.Variable(name, _, _) =>
        CalculationSummary(name, v.showValue).formatValue

      @scala.annotation.tailrec
      def extractVariables(acc: List[CalculationSummary], calculations: List[CalculationDescription]): List[CalculationSummary] =
        calculations match
          case (v @ Calculation.Variable(_, _, _)) :: rest =>
            extractVariables(CalculationSummary(v.name, v.showValue) :: acc, rest)
          case other :: rest =>
            extractVariables(acc, other.inputs.toList ::: rest)
          case Nil => acc

      @scala.annotation.tailrec
      def extractBindings(acc: List[String], calculations: List[CalculationDescription]): List[String] =
        calculations match
          case Calculation.Binding(name, expression, _) :: rest =>
            val nameIndent = " " * name.size
            val equations = List(
              "",
              s"$name = ${equationWithNames(expression)}",
              s"$nameIndent = ${equationWithValues(expression)}",
              s"$nameIndent = ${expression.showValue}"
            ).mkString("\n")
            extractBindings(equations :: acc, expression.inputs.toList ::: rest)
          case other :: rest =>
            extractBindings(acc, other.inputs.toList ::: rest)
          case Nil => acc

      val variables = extractVariables(Nil, calculation.inputs.toList)
        .distinct
        .sortBy(_.name)
        .map(_.formatValue)

      val bindings = extractBindings(Nil, List(calculation))
        .distinct

      val lines = Seq(heading, "") ++ 
        Seq("VARIABLES:") ++ 
        variables ++
        Seq("", "CALCULATION STEPS:") ++
        bindings
      lines.mkString("\n")