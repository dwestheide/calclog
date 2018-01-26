package calclog

import calclog.Calculation.{Binding, Expression, Variable}

private[calclog] object Equation {

  private[calclog] def equation[A](expression: Expression[A]): String = {
    val left = describeOp(expression.op, outmost = true)
    left + " = " + expression.showValue
  }

  private[calclog] def describeOp(op: OpDescription, outmost: Boolean): String = {
    def descCalc(calc: CalculationDescription): String = calc match {
      case Variable(name, _) => name
      case Binding(name, _) => name
      case Expression(op1) => describeOp(op1, outmost = false)
    }
    op match {
      case infix: OpDescription.Infix =>
        val l = descCalc(infix.left)
        val r = descCalc(infix.right)
        if (outmost) s"$l ${infix.symbol} $r" else s"($l ${infix.symbol} $r)"
      case func: OpDescription.Func =>
        val inputs = func.inputs.map(descCalc)
        if (outmost) s"${func.name}(${inputs.mkString(", ")})"
        else s"(${func.name}(${inputs.mkString(", ")}))"
    }
  }

}
