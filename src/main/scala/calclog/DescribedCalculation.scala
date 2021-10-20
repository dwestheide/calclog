package calclog

final case class DescribedCalculation[A](
  value: Evaluated[A], 
  description: CalculationDescription
)
