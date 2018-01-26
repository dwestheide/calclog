package calclog.examples.quadraticroots

import calclog.{Calculation, Format}

object QuadraticRootsExample extends App {

  import calclog.FormatSyntax._
  import calclog.Implicits._
  implicit val doubleFormat: Format[Double] = _.toString

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

    val four = 4d ~ "4"
    val two = 2d ~ "2"
    val minusOne = -1d ~ "-1"

    println((four * four + two).run.description.format)

    val bSquared = b * b <~ "b^2"
    val fourac = four * a * c <~ "4ac"
    val determinant = bSquared - fourac <~ "determinant"
    val sqrtDeterminant = determinant.sqrt <~ "sqrt(determinant)"
    val minusB = b * minusOne <~ "-b"
    val numerator = minusB + sqrtDeterminant <~ "numerator"
    val denominator = two * a <~ "denominator"
    numerator / denominator <~ "root"

  }
}
