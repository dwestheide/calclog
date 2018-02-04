package calclog

import scala.annotation.implicitNotFound

@implicitNotFound("\nCannot find a ValueFormatter instance for type ${A}.\n" +
  "If you are happy with the behaviour of toString, you can fix this by\n" +
  "importing calclog.ValueFormatter.Implicits.automaticToStringValueFormatter.\n\n" +
  "Alternatively, you can define your own formatting behaviour for values of\n" +
  "type ${A} by defining an implicit instance of calclog.ValueFormatter[${A}].\n\n")
trait ValueFormatter[A] {
  def format(a: A): String
}
object ValueFormatter {

  def apply[A](implicit show: ValueFormatter[A]): ValueFormatter[A] = show

  def format[A: ValueFormatter](a: A): String = ValueFormatter[A].format(a)

  def toStringValueFormatter[A]: ValueFormatter[A] = _.toString

  object Implicits {
    implicit def automaticToStringValueFormatter[A]: ValueFormatter[A] = toStringValueFormatter
  }


}

trait ValueFormatterSyntax {
  implicit class ValueFormatterOps[A: ValueFormatter](a: A) {
    def formatValue: String = ValueFormatter.format(a)
  }
}

object ValueFormatterSyntax extends ValueFormatterSyntax