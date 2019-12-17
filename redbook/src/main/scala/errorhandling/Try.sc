import scala.util.{Try,Success,Failure}

object app {

  def sumCirclesDiv(rs: List[String], d: Int = 1): Int = {
    val areas = rs.map(r => { math.Pi * r.toDouble * r.toDouble })
    val divs = areas.map(_.toInt / d)
    divs.sum
  }

  def safeSumCirclesDiv(rs: List[String], d: Int = 1): Int = {
    try {
      val areas = rs.map((r) => { math.Pi * r.toDouble * r.toDouble })
      val divs = areas.map(_.toInt / d)
      divs.sum
    } catch {
      case x: ArithmeticException => {
        println("you cannot divide by " + d)
        0
      }
      case x: NumberFormatException => {
        println("you must provide parseable numbers")
        0
      }
      case unknown: Throwable => {
        println("Exception: " + unknown)
        0
      }
    }
  }
  
  def saferSumCirclesDiv(rs: List[String], d: Int = 1): Int = {
    var acc = 0
    for (r <- rs) {
      try {
        val area = math.Pi * r.toDouble * r.toDouble
        acc += area.toInt / d
      } catch {
        case x: ArithmeticException => {
          println("you cannot divide by " + d)
        }
        case x: NumberFormatException => {
          println("you must provide parseable numbers")
        }
        case unknown: Throwable => {
          println("Exception: " + unknown)
        }
      }
    }
    acc
  }
  
  def optionSumCirclesDiv(rs: List[String], d: Int = 1): Option[Int] = {
    
    def optionDouble(s: String) = 
      try { Some(s.toDouble) } catch { case _: Throwable => None }
    
    def circled(r: Double) = Some(math.Pi * r * r)
    
    def divN(y: Int)(x: Double) =
      try { Some(x.toInt / y) } catch { case _: Throwable => None }
    
    rs.flatMap(optionDouble).flatMap(circled).flatMap(divN(d)) match {
      case Nil => None
      case x => Some(x.sum)
    }
  }
  
  def eitherSumCirclesDiv(rs: List[String], d: Int = 1): Either[String, Int] = {
        
    def eitherDouble(s: String) = 
      try { Right(s.toDouble) } 
      catch { case _: Throwable => Left("Cannot parse " + s) }
    
    def circled(r: Double) = Right(math.Pi * r * r)
    
    def divN(y: Int)(x: Double) =
      try { Right(x.toInt / y) } 
      catch { case _: Throwable => Left("Cannot divide by " + y.toString) }
    
    val list = rs.map(eitherDouble).map(_.flatMap(circled)).map(_.flatMap(divN(d)))
    
    list.collect{ case Left(x) => x }.map(println)
    list.collect{ case Right(x) => x } match {
      case Nil => Left("No parseable numbers to compute")
      case x => Right(x.sum)
    }
  }

  def trySumCirclesDiv(rs: List[String], d: Int = 1): Try[Int] = {    
    def tryDivArea(d: Int = 1)(s: String) =
      Try({
        val r = s.toDouble        
        val a = math.Pi * r * r
        a.toInt / d
      })
    val all = rs.map(tryDivArea(d))
    all.filter(_.isFailure).map(println)
    all.filter(_.isSuccess).map(_.get) match {
      case Nil => Failure(new Exception("Empty List"))
      case x => Try(x.sum)
    }
  }
  
}

import app.{_}

val foobar = List("1", "2.5", "4.1")
sumCirclesDiv(foobar)
safeSumCirclesDiv(foobar)
saferSumCirclesDiv(foobar)
optionSumCirclesDiv(foobar)
// Exception! sumCirclesDiv(foobar, 0)
safeSumCirclesDiv(foobar, 0)
saferSumCirclesDiv(foobar, 0)
optionSumCirclesDiv(foobar, 0)

val fubar = List("1", "abc", "2.5", "one", "2.t", "4.1")
// Exception! sumCirclesDiv(fubar)
safeSumCirclesDiv(fubar)
saferSumCirclesDiv(fubar)

// option
optionSumCirclesDiv(fubar)
optionSumCirclesDiv(fubar, 0)
optionSumCirclesDiv(List("none"))
optionSumCirclesDiv(List("0"))

// either 
eitherSumCirclesDiv(fubar)
eitherSumCirclesDiv(fubar, 0)
eitherSumCirclesDiv(List("none"))
eitherSumCirclesDiv(List("0"))

// try 
trySumCirclesDiv(fubar)
trySumCirclesDiv(fubar, 0)
trySumCirclesDiv(List("none"))
trySumCirclesDiv(List("0"))

