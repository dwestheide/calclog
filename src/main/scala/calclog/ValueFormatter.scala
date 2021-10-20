package calclog

trait ValueFormatter[A]:
  def format(a: A): String
  extension (a: A) def formatValue: String = format(a)

object ValueFormatter:

  def apply[A](using formatter: ValueFormatter[A]): ValueFormatter[A] = formatter
  def toStringValueFormatter[A]: ValueFormatter[A] = _.toString

  object Defaults:
    given [A]: ValueFormatter[A] = ValueFormatter.toStringValueFormatter

