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

object datetime extends DateTimeOperatorInstances {
  object fractions {
    object bigdecimal {
      implicit val durationFractionAsBigDecimal: calclog.Divide[Duration, Duration, BigDecimal] =
        calclog.Divide.safe((d1, d2) => BigDecimal(d1.toNanos) / BigDecimal(d2.toNanos))
    }
    object double {
      implicit val durationFractionAsDouble: calclog.Divide[Duration, Duration, Double] =
        calclog.Divide.safe((d1, d2) => d1.toNanos.toDouble / d2.toNanos)
    }
  }
}
