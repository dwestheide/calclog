package calclog

import calclog.Calculation.{Binding, Expression, Variable}

trait Dsl {

  implicit class RichExpression[A: Format](e: Expression[A]) {
    def <~(name: String): Calculation[A] = Binding(name, e)
  }

  implicit class ToVariable[A: Format](a: A) {
    def ~(name: String): Calculation[A] = Variable(name, a)
  }

  def define[A: Format](name: String)(e: Expression[A]): Calculation[A] = Binding(name, e)

}

object Dsl extends Dsl