package calclog

import calclog.Calculation.{Binding, Expression, Variable}

trait Dsl {

  implicit class RichExpression[A](e: Expression[A]) {
    def <~(name: String)(implicit formatter: ValueFormatter[A]): Calculation[A] = Binding(name, e)
  }

  implicit class ToVariable[A](a: A) {
    def ~(name: String)(implicit formatter: ValueFormatter[A]): Calculation[A] = Variable(name, a)
  }

  def define[A: ValueFormatter](name: String)(e: Expression[A]): Calculation[A] = Binding(name, e)

  implicit class RichLiteral[A: ValueFormatter](a: A) {
    def literal: Calculation[A] = Expression(Op.Literal(a))
  }

}

object Dsl extends Dsl