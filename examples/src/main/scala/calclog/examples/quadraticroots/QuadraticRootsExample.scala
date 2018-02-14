package calclog.examples.quadraticroots

import calclog.Calculation

object QuadraticRootsExample extends App {
  
  import calclog.CalculationFormatter.syntax._
  import calclog.Implicits._
  import calclog.ValueFormatter.Implicits._

  final case class Parameters(a: Double, b: Double, c: Double)

//  println("Success case:")
  println(root(Parameters(2, 5, 3)).run.description.format)
//  println()
//  println("Failure case:")
//  println(root(Parameters(2, 5, 10)).run.description.format)

  def root(parameters: Parameters): Calculation[Double] = {
    val a = parameters.a ~ "a"
    val b = parameters.b ~ "b"
    val c = parameters.c ~ "c"

    val fourac = 4d.literal * a * c <~ "4ac"
    val determinant = b * b - fourac <~ "determinant"
    val numerator = -b + sqrt(determinant) <~ "numerator"
    val denominator = 2d.literal * a <~ "denominator"
    numerator / denominator <~ "root"
  }
}
