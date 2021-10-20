package calclog

trait Dsl:
  extension [A: ValueFormatter](e: Calculation.Expression[A])
    def <~(name: String): Calculation[A] = Calculation.Binding(name, e)
  
  extension [A](a: A)(using format: ValueFormatter[A])
    def ~(name: String): Calculation[A] = Calculation.Variable(name, a)
    def literal: Calculation[A] = Calculation.Expression(Op.Literal(a, format))

  def define[A: ValueFormatter](name: String)(e: Calculation.Expression[A]): Calculation[A] = Calculation.Binding(name, e)

object Dsl extends Dsl