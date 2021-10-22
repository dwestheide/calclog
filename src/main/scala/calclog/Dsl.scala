package calclog

trait Dsl:
  extension [A](e: Calculation.Expression[A])(using format: ValueFormatter[A])
    def <~(name: String): Calculation[A] = Calculation.Binding(name, e, format)
  
  extension [A](a: A)(using format: ValueFormatter[A])
    def ~(name: String): Calculation[A] = Calculation.Variable(name, a, format)
    def literal: Calculation[A] = Calculation.Expression(Op.Literal(a, format), format)

  def define[A](using format: ValueFormatter[A])(name: String)(e: Calculation.Expression[A]): Calculation[A] = Calculation.Binding(name, e, format)

object Dsl extends Dsl