package calclog

import scala.collection.immutable.Seq

sealed trait OpDescription {
  def inputs: Seq[CalculationDescription]
}

object OpDescription {

  sealed trait Infix extends OpDescription {
    def left: CalculationDescription
    def right: CalculationDescription
    def symbol: String
  }

  sealed trait Func extends OpDescription {
    def name: String
  }
}

sealed trait Op[A] extends OpDescription {
  def evaluated: Evaluated[A]
}

object Op {

  final case class Infix[A, B, C](left: Calculation[A],
                                  right: Calculation[B],
                                  f: (A, B) => Evaluated[C],
                                  symbol: String)
      extends OpDescription.Infix
      with Op[C] {

    override def evaluated: Evaluated[C] =
      for {
        a <- left.extractValue
        b <- right.extractValue
        c <- f(a, b)
      } yield c

    override def inputs: Seq[CalculationDescription] = Seq(left, right)
  }

  final case class OneArgFunction[A, B](x: Calculation[A],
                                        f: A => Evaluated[B],
                                        name: String)
      extends OpDescription.Func
      with Op[B] {
    override def evaluated: Evaluated[B] =
      for {
        a <- x.extractValue
        b <- f(a)
      } yield b
    override def inputs: Seq[CalculationDescription] = Seq(x)
  }

}
