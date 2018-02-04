package calclog

trait CalculationFormatter[A] {
  def format(a: A): String
}
object CalculationFormatter {

  def apply[A](implicit show: CalculationFormatter[A]): CalculationFormatter[A] = show

  def format[A: CalculationFormatter](a: A): String = CalculationFormatter[A].format(a)

  object syntax extends CalculationFormatterSyntax

}

trait CalculationFormatterSyntax {
  implicit class CalculationFormatterOps[A: CalculationFormatter](a: A) {
    def format: String = CalculationFormatter.format(a)
  }
}