package calclog

trait Format[A] {
  def format(a: A): String
}
object Format {

  def apply[A](implicit show: Format[A]): Format[A] = show

  def format[A: Format](a: A): String = Format[A].format(a)

  implicit val formatBigDecimal: Format[BigDecimal] = _.toString()

}

trait FormatSyntax {
  implicit class FormatOps[A: Format](a: A) {
    def format: String = Format.format(a)
  }
}

object FormatSyntax extends FormatSyntax