package calclog

private[calclog] def equation[A](expression: Calculation.Expression[A]): String =
    val left = describeOp(expression.op.description, outmost = true)
    left + " = " + expression.showValue

private[calclog] def equationWithValues[A](expression: Calculation.Expression[A]): String =
    describeOp(expression.op.description, outmost = true, useNames = false)

private[calclog] def equationWithNames[A](expression: Calculation.Expression[A]): String =
    describeOp(expression.op.description, outmost = true, useNames = true)
  
private[calclog] def describeOp[A](op: OpDescription, outmost: Boolean, useNames: Boolean = true): String =
  def descCalc(calc: CalculationDescription): String = calc match
      case v @ Calculation.Variable(name, _) => if (useNames) name else v.showValue
      case b @ Calculation.Binding(name, expression)  => if (useNames) name else b.showValue
      case Calculation.Expression(op1)   => describeOp(op1.description, outmost = false, useNames)
  
  op match
    case infix: OpDescription.Infix =>
      val l = descCalc(infix.left)
      val r = descCalc(infix.right)
      if (outmost) s"$l ${infix.symbol} $r" else s"($l ${infix.symbol} $r)"
    case func: OpDescription.Func =>
      val inputs = func.inputs.map(descCalc)
      if (outmost) s"${func.name}(${inputs.mkString(", ")})"
      else s"(${func.name}(${inputs.mkString(", ")}))"
    case literal: OpDescription.Literal =>
      literal.valueString
    case unary: OpDescription.Unary =>
      s"${unary.symbol}${descCalc(unary.operand)}"
