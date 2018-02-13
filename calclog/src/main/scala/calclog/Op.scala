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

  sealed trait Literal extends OpDescription {
    def valueString: String
    final override def inputs: Seq[CalculationDescription] = Seq.empty
  }

  sealed trait Unary extends OpDescription {
    def symbol: String
    def operand: CalculationDescription
    final override def inputs: Seq[CalculationDescription] = Seq(operand)
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

  final case class Literal[A](value: A)(implicit valueFormatter: ValueFormatter[A]) extends OpDescription.Literal with Op[A] {
    override def evaluated: Evaluated[A] = Evaluated.success(value)
    override def valueString: String = valueFormatter.format(value)
  }

  final case class Unary[A](operand: Calculation[A], f: A => Evaluated[A], symbol: String)
    extends OpDescription.Unary with Op[A] {
    override def evaluated: Evaluated[A] = for {
      a <- operand.extractValue
      a2 <- f(a)
    } yield a2
  }

}
