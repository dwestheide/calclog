package calclog

trait Defaults extends Dsl, DefaultOperatorSyntax, DefaultOperatorInstances:
  given [A]: ValueFormatter[A] = ValueFormatter.toStringValueFormatter
object Defaults extends Defaults
