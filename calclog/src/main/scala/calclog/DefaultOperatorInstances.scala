package calclog

trait DefaultOperatorInstances {

  implicit val bigDecimalAddition: Plus[BigDecimal] = Plus.safe(_ + _)
  implicit val integerAddition: Plus[Int] = Plus.safe(_ + _)
  implicit val longAddition: Plus[Long] = Plus.safe(_ + _)
  implicit val floatAddition: Plus[Float] = Plus.safe(_ + _)
  implicit val doubleAddition: Plus[Double] = Plus.safe(_ + _)

  implicit def numericSubtraction[A: Numeric]: Minus[A] =
    Minus.safe(implicitly[Numeric[A]].minus(_, _))

  implicit def numericMultiplication[A: Numeric]: Times[A, A, A] =
    Times.safe(implicitly[Numeric[A]].times(_, _))

  implicit def fractionalDivision[A: Fractional]: Divide[A, A, A] =
    Divide.safe(implicitly[Fractional[A]].div(_, _))

  implicit val doubleSqareRoot: SquareRoot[Double] =
    SquareRoot.safe(scala.math.sqrt).filterOrElse(!_.isNaN, "NaN")

}