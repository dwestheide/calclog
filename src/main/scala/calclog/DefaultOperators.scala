package calclog

trait Plus[A] extends ((A, A) => Evaluated[A])
object Plus:
  def safe[A](f: (A, A) => A): Plus[A] = (x: A, y: A) => Evaluated(f(x, y))
  def apply[A](using plus: Plus[A]): Plus[A] = plus

trait Minus[A] extends ((A, A) => Evaluated[A])
object Minus:
  def safe[A](f: (A, A) => A): Minus[A] = (x: A, y: A) => Evaluated(f(x, y))
  def apply[A](using minus: Minus[A]): Minus[A] = minus

trait Times[A, B, C] extends ((A, B) => Evaluated[C])
object Times:
  def safe[A, B, C](f: (A, B) => C): Times[A, B, C] = (x: A, y: B) => Evaluated(f(x, y))
  def apply[A, B, C](using times: Times[A, B, C]): Times[A, B, C] = times

trait Divide[A, B, C] extends ((A, B) => Evaluated[C])
object Divide:
  def safe[A, B, C](f: (A, B) => C): Divide[A, B, C] = (x: A, y: B) => Evaluated(f(x, y))
  def apply[A, B, C](using divide: Divide[A, B, C]): Divide[A, B, C] = divide

trait SquareRoot[A] extends (A => Evaluated[A]):
  self =>
  def filterOrElse(f: A => Boolean, error: String): SquareRoot[A] = a => self(a).filterOrElse(f, error)

object SquareRoot:
  def safe[A](f: A => A): SquareRoot[A] = (x: A) => Evaluated(f(x))
  def apply[A](using sqrt: SquareRoot[A]): SquareRoot[A] = sqrt

trait UnaryMinus[A] extends (A => Evaluated[A])
object UnaryMinus:
  def safe[A](f: A => A): UnaryMinus[A] = a => Evaluated(f(a))
  def apply[A](using unaryMinus: UnaryMinus[A]): UnaryMinus[A] = unaryMinus
