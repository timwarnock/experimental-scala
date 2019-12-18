package errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {

  /** 4.6
   * scala> def circled(r: Double) = math.Pi * r * r
   * circled: (r: Double)Double
   *
   * scala> Right(2.5).map(circled)
   * res0: errorhandling.Either[Nothing,Double] = Right(19.634954084936208)
   *
   * scala> Right(4.2).flatMap((r: Double) => Right(math.Pi * r * r))
   * res1: errorhandling.Either[Nothing,Double] = Right(55.41769440932395)
   *
   * scala> res0.getOrElse(0.0) + res1.getOrElse(0.0)
   * res2: Double = 75.05264849426015
   *
   * // since we have map and flatMap we can use for-comprehension
   * scala> for {
   *      |  x <- Right(4.1).map(circled)
   *      |  y <- Right(2.5).map(circled)
   *      | } yield x + y
   * res3: errorhandling.Either[Nothing,Double] = Right(72.44512659178062)
   *
   * scala> Right(4.1).map2(Right(2.5))((a,b) => circled(a) + circled(b))
   * res4: errorhandling.Either[Nothing,Double] = Right(72.44512659178062)
   *
   */
  def getOrElse[A1 >: A](or: => A1): A1 = this match {
    case Right(a) => a
    case _        => or
  }
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  /**
   * scala> Either.mean(IndexedSeq(1,2,3,4))
   * res0: errorhandling.Either[String,Double] = Right(2.5)
   *
   */
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  /**
   * scala> Either.safeDiv(42, 0)
   * res0: errorhandling.Either[Exception,Int] = Left(java.lang.ArithmeticException: / by zero)
   *
   */
  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  /**
   * scala> Either.Try(42 / 0)
   * res0: errorhandling.Either[Exception,Int] = Left(java.lang.ArithmeticException: / by zero)
   *
   */
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /** 4.7
   *
   * scala> Either.traverse(List(3,2,1))(x => Either.Try(42/x))
   * res0: errorhandling.Either[Exception,List[Int]] = Right(List(14, 21, 42))
   *
   * scala> Either.traverse(List(3,2,1,0))(x => Either.Try(42/x))
   * res1: errorhandling.Either[Exception,List[Int]] = Left(java.lang.ArithmeticException: / by zero)
   *
   * scala> Either.sequence(List(Right(1), Right(2), Right(3)))
   * res2: errorhandling.Either[Nothing,List[Int]] = Right(List(1, 2, 3))
   *
   * scala> Either.sequence(List(Right(1), Left(2), Left(3)))
   * res3: errorhandling.Either[Int,List[Int]] = Left(2)
   *
   */
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  /** 4.8 (from answer sheet)
  There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
  approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

  trait Partial[+A,+B]
  case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
  case class Success[+B](get: B) extends Partial[Nothing,B]

  There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
  `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
  accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
  values into a list; we can accumulate values using any user-supplied binary function.

  */
}
