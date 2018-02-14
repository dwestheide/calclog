package calclog

import java.time.Duration

trait DateTimeOperatorInstances {
  implicit val durationAddition: Plus[Duration] = Plus.safe(_.plus(_))
  implicit val durationSubtraction: Minus[Duration] = Minus.safe(_.minus(_))
  implicit val durationTimesInt: Times[Duration, Int, Duration] = Times.safe((d, a) => d.multipliedBy(a))
  implicit val durationTimesLong: Times[Duration, Long, Duration] = Times.safe((d, a) => d.multipliedBy(a))
  implicit val durationByInt: Divide[Duration, Int, Duration] = Divide.safe((d, a) => d.dividedBy(a))
  implicit val durationByLong: Divide[Duration, Long, Duration] = Divide.safe((d, a) => d.dividedBy(a))
}

object datetime extends DateTimeOperatorInstances
