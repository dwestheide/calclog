package calclog

import scala.util.control.NonFatal

type Evaluated[A] = Either[String, A]

object Evaluated:
  def apply[A](a: => A): Evaluated[A] = 
    try success(a)
    catch case NonFatal(e) => failed(e.getMessage)
  
  def failed[A](reason: String): Evaluated[A] = Left(reason)
  def success[A](value: A): Evaluated[A] = Right(value)

