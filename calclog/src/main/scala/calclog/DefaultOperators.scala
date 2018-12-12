package calclog

import scala.annotation.implicitNotFound

@implicitNotFound("\nCannot add values of type ${A}.\n" +
  "You may need to extend calclog.DefaultOperatorInstances or\n" +
  "import calclog.Implicits._.\n\n" +
  "If A is a custom type, you will probably need to implement your own\n" +
  "instance of the Plus typeclass for it."
)
trait Plus[A] extends (A, A) => Evaluated[A]

object Plus {
  def safe[A](f: (A, A) => A): Plus[A] = (x: A, y: A) => Evaluated(f(x, y))
  def apply[A](implicit plus: Plus[A]): Plus[A] = plus
}

@implicitNotFound("\nCannot subtract values of type ${A}.\n" +
  "You may need to extend calclog.DefaultOperatorInstances or\n" +
  "import calclog.Implicits._.\n\n" +
  "If A is a custom type, you will probably need to implement your own\n" +
  "instance of the Minus typeclass for it."
)
trait Minus[A] extends (A, A) => Evaluated[A]

object Minus {
  def safe[A](f: (A, A) => A): Minus[A] = (x: A, y: A) => Evaluated(f(x, y))
  def apply[A](implicit minus: Minus[A]): Minus[A] = minus
}

@implicitNotFound("\nCannot multiply values of type ${A} and ${B}.\n" +
  "You may need to extend calclog.DefaultOperatorInstances or\n" +
  "import calclog.Implicits._.\n\n" +
  "If A or B is a custom type, you will probably need to implement your own\n" +
  "instance of the Times typeclass."
)
trait Times[A, B, C] extends (A, B) => Evaluated[C]

object Times {
  def safe[A, B, C](f: (A, B) => C): Times[A, B, C] = (x: A, y: B) => Evaluated(f(x, y))
  def apply[A, B, C](implicit times: Times[A, B, C]): Times[A, B, C] = times
}

@implicitNotFound("\nCannot divide value of type ${A} by value of type ${B}.\n" +
  "You may need to extend calclog.DefaultOperatorInstances or\n" +
  "import calclog.Implicits._.\n\n" +
  "If A or B is a custom type, you will probably need to implement your own\n" +
  "instance of the Divide typeclass."
)
trait Divide[A, B, C] extends (A, B) => Evaluated[C]

object Divide {
  def safe[A, B, C](f: (A, B) => C): Divide[A, B, C] = (x: A, y: B) => Evaluated(f(x, y))
  def apply[A, B, C](implicit divide: Divide[A, B, C]): Divide[A, B, C] = divide
}

@implicitNotFound("\nCannot calculate square root for values of type ${A}.\n" +
  "You may need to extend calclog.DefaultOperatorInstances or\n" +
  "import calclog.Implicits._.\n\n" +
  "If A is a custom type, you will probably need to implement your own\n" +
  "instance of the SquareRoot typeclass for it."
)
trait SquareRoot[A] extends A => Evaluated[A] { self =>
  def filterOrElse(f: A => Boolean, error: String): SquareRoot[A] = a => self(a).filterOrElse(f, error)
}

object SquareRoot {
  def safe[A](f: A => A): SquareRoot[A] = (x: A) => Evaluated(f(x))
  def apply[A](implicit sqrt: SquareRoot[A]): SquareRoot[A] = sqrt
}

@implicitNotFound("\nCannot negate values of type ${A}.\n" +
  "You may need to extend calclog.DefaultOperatorInstances or\n" +
  "import calclog.Implicits._.\n\n" +
  "If A is a custom type, you will probably need to implement your own\n" +
  "instance of the UnaryMinus typeclass for it."
)
trait UnaryMinus[A] extends A => Evaluated[A]

object UnaryMinus {
  def safe[A](f: A => A): UnaryMinus[A] = a => Evaluated(f(a))
  def apply[A](implicit unaryMinus: UnaryMinus[A]): UnaryMinus[A] = unaryMinus
}
