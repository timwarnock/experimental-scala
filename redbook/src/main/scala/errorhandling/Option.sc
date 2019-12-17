
object app {

  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a: A) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

  }
}

// hide the old
import scala.{Option => _}

// import the new
import app.{_}


val foobar = Some("1.5")
val fubar = None
fubar.getOrElse(0.0)
foobar.map((x: String) => x.toDouble + 1.0)

