package calclog

object ToStringFormat {

  def toStringFormat[A]: Format[A] = _.toString

  object Implicits {
    implicit def automaticToStringFormat[A]: Format[A] = toStringFormat
    implicit val intFormat: Format[Int] = ToStringFormat.toStringFormat
    implicit val doubleFormat: Format[Double] = ToStringFormat.toStringFormat
    implicit val bigDecimalFormat: Format[BigDecimal] = ToStringFormat.toStringFormat
    implicit val longFormat: Format[Long] = ToStringFormat.toStringFormat
    implicit val floatFormat: Format[Float] = ToStringFormat.toStringFormat
  }

}
