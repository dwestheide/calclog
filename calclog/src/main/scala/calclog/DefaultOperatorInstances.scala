package calclog

import java.time.Duration

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

  implicit val durationAddition: Plus[Duration] = Plus.safe(_.plus(_))
  implicit val durationSubtraction: Minus[Duration] = Minus.safe(_.minus(_))
  implicit def durationTimesNumber[A](implicit numericA: Numeric[A]): Times[Duration, A, Duration] =
    Times.safe((d, a) => d.multipliedBy(numericA.toLong(a)))
  implicit def durationByNumberDivision[A](implicit numericA: Numeric[A]): Divide[Duration, A, Duration] =
    Divide.safe((d, a) => d.dividedBy(numericA.toLong(a)))


}