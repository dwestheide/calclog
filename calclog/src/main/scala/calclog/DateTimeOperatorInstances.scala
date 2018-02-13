package calclog

import java.time.Duration

trait DateTimeOperatorInstances {
  implicit val durationAddition: Plus[Duration] = Plus.safe(_.plus(_))
  implicit val durationSubtraction: Minus[Duration] = Minus.safe(_.minus(_))
  implicit def durationTimesNumber[A](implicit numericA: Numeric[A]): Times[Duration, A, Duration] =
    Times.safe((d, a) => d.multipliedBy(numericA.toLong(a)))
  implicit val durationByInt: Divide[Duration, Int, Duration] = Divide.safe((d, a) => d.dividedBy(a))
  implicit val durationByLong: Divide[Duration, Long, Duration] = Divide.safe((d, a) => d.dividedBy(a))
}

object datetime extends DateTimeOperatorInstances
