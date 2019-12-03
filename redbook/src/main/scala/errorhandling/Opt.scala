package errorhandling
/**
 * you can run this in the REPL using :paste rather than :load
 * e.g.,
 *
 * scala> :paste errorhandling/Opt.scala
 * scala> import errorhandling._
 * scala> Som("1.5").map(_.toDouble)
 * res0: errorhandling.Opt[Double] = Som(1.5)
 *
 * IF you want to match the built-in names,
 * scala> import errorhandling.{Opt => Option, Non => None, Som => Some}
 *
 */

case object Non extends Opt[Nothing]
case class Som[+A](get: A) extends Opt[A]

sealed trait Opt[+A] {
  /** 4.1
   *
   * scala> val foo = Som("1.5")
   *
   * scala> foo.map(_.toDouble + 1)
   * res0: errorhandling.Opt[Double] = Som(2.5)
   *
   * scala> foo.getOrElse(0)
   * res1: Any = 1.5
   *
   * scala> foo.flatMap((x: String) => Som(x.toDouble))
   * res2: errorhandling.Opt[Double] = Som(1.5)
   *
   * scala> foo.orElse(Som("ok"))
   * res3: errorhandling.Opt[String] = Som(1.5)
   *
   * scala> foo.filter(_.length > 5)
   * res4: errorhandling.Opt[String] = Non
   *
   */
  def map[B](f: A => B): Opt[B] = this match {
    case Non => Non
    case Som(a) => Som(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Non => default
    case Som(a) => a
  }

  def flatMap[B](f: A => Opt[B]): Opt[B] =
    map(f) getOrElse Non

  def orElse[B >: A](ob: => Opt[B]): Opt[B] =
    this map (Som(_)) getOrElse ob

  def filter(f: A => Boolean): Opt[A] = this match {
    case Som(a) if f(a) => this
    case _ => Non
  }
}



object Opt {

  /**
   * scala> Opt.mean(Seq(1.0,2.0))
   * res2: Opt[Double] = Som(1.5)
   *
   */
  def mean(xs: Seq[Double]): Opt[Double] =
    if (xs.isEmpty) Non
    else Som(xs.sum / xs.length)


  /** 4.2
   *
   * scala> Opt.variance(Seq(1.5,1,0,-1))
   * res0: errorhandling.Opt[Double] = Som(0.921875)
   *
   */
  def variance(xs: Seq[Double]): Opt[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


  /**
   *
   * scala> val absO: Opt[Double] => Opt[Double] = Opt.lift(math.abs)
   * scala> absO(Som(-1.5))
   * res0: errorhandling.Opt[Double] = Som(1.5)
   *
   * scala> Opt.lift(math.abs)(Som(-1))
   * res1: errorhandling.Opt[Int] = Som(1)
   *
   */
  def lift[A,B](f: A => B): Opt[A] => Opt[B] = _ map f


  /** 4.3
   *
   * scala> Opt.map2(Som(1), Som(2))(_+_)
   * res0: errorhandling.Opt[Int] = Som(3)
   *
   */
  def map2[A,B,C](a: Opt[A], b: Opt[B])(f: (A, B) => C): Opt[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2_for[A,B,C](a: Opt[A], b: Opt[B])(f: (A, B) => C): Opt[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)


  /** 4.4
   *
   * scala> Opt.sequence(List(Som(1), Som(2), Som(3)))
   * res0: errorhandling.Opt[List[Int]] = Som(List(1, 2, 3))
   *
   * scala> Opt.sequence(List(Som(1), Som(2), Non, Som(3)))
   * res1: errorhandling.Opt[List[Int]] = Non
   *
   */
  def sequence[A](a: List[Opt[A]]): Opt[List[A]] =
    a match {
      case Nil => Som(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def listify[A](a: List[Opt[A]], default: A): List[A] =
    for (x <- a) yield x.getOrElse(default)


  /** 4.5
   *
   * scala> Opt.sequence_1(List(Som(1), Som(2), Som(3)))
   * res0: errorhandling.Opt[List[Int]] = Som(List(1, 2, 3))
   *
   * scala> Opt.traverse(List("1", "2", "3"))(x => Som(x.toInt))
   * res1: errorhandling.Opt[List[Int]] = Som(List(1, 2, 3))
   *
   */
  def sequence_1[A](a: List[Opt[A]]): Opt[List[A]] =
    a.foldRight[Opt[List[A]]](Som(Nil))((x, y) => map2(x,y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Opt[B]): Opt[List[B]] =
    a match {
      case Nil => Som(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Opt[B]): Opt[List[B]] =
    a.foldRight[Opt[List[B]]](Som(Nil))((h, t) => map2(f(h),t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Opt[A]]): Opt[List[A]] =
    traverse(a)(x => x)
}
