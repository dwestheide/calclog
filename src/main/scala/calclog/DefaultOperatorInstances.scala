package calclog

trait DefaultOperatorInstances:

  given [A](using Numeric[A]): Plus[A] with
    def apply(x: A, y: A): Evaluated[A] = Evaluated(Numeric[A].plus(x, y))

  given [A](using Numeric[A]): Minus[A] with
    def apply(x: A, y: A): Evaluated[A] = Evaluated(Numeric[A].minus(x, y))

  given [A](using Numeric[A]): Times[A, A, A] with
    def apply(x: A, y: A): Evaluated[A] = Evaluated(Numeric[A].times(x, y))

  given Times[BigDecimal, Int, BigDecimal] with
    def apply(x: BigDecimal, y: Int): Evaluated[BigDecimal] = Evaluated(x * y)

  given Times[Int, BigDecimal, BigDecimal] with
    def apply(x: Int, y: BigDecimal): Evaluated[BigDecimal] = Evaluated(x * y)

  given [A](using fractional: Fractional[A]): Divide[A, A, A] with
    def apply(x: A, y: A): Evaluated[A] = Evaluated(fractional.div(x, y))

  given Divide[Int, Int, Int] with
    def apply(x: Int, y: Int): Evaluated[Int] = Evaluated(x / y)
  
  given SquareRoot[Double] with
    def apply(x: Double): Evaluated[Double] = 
      Evaluated(scala.math.sqrt(x)).filterOrElse(!_.isNaN, "NaN")
  
  given [A](using Numeric[A]): UnaryMinus[A] with
    def apply(x: A): Evaluated[A] = Evaluated(Numeric[A].negate(x))

object DefaultOperatorInstances extends DefaultOperatorInstances