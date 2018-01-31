package calclog

trait DefaultOperatorInstances {

  implicit def numericAddition[A: Numeric]: Plus[A] =
    Plus.safe(implicitly[Numeric[A]].plus(_, _))

  implicit def numericSubtraction[A: Numeric]: Minus[A] =
    Minus.safe(implicitly[Numeric[A]].minus(_, _))

  implicit def numericMultiplication[A: Numeric]: Times[A, A, A] =
    Times.safe(implicitly[Numeric[A]].times(_, _))

  implicit def fractionalDivision[A: Fractional]: Divide[A, A, A] =
    Divide.safe(implicitly[Fractional[A]].div(_, _))

  implicit val doubleSqareRoot: SquareRoot[Double] =
    SquareRoot.safe(scala.math.sqrt).filterOrElse(!_.isNaN, "NaN")

}