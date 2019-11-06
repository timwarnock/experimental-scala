package gettingstarted

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

}

object Monomorphic {

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    loop(0)
  }

}

object Polymorphic {

  /**  scala> Polymorphic.findFirst(Array("abc", "b", "c"), (x: String) => x == "b")
   *   res1: Int = 1
   */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  /**  scala> Polymorphic.isSorted(Array(1,2,3), (x: Int, y: Int) => x > y)
   *   res1: Boolean = true
   */
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)
    go(0)
  }

  /**  scala> partial1(" argh!", (a: String, b: String) => b + a)("hello")
   *   res1: String = hello argh!
   */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  /**  scala> val strlen2 = (a: String, b: String) => a.length() + b.length()
   *
   *   scala> val strlenAdd5 = curry(strlen2)("five!")
   *    -OR-
   *   scala> val strlenAdd5 = strlen2.curried("five!")
   *
   *   scala> strlenAdd5("sixsix")
   *   res1: Int = 11
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  /**  scala> val strlen2 = (a: String, b: String) => a.length() + b.length()
   *   scala> val curryLen = curry(strlen2)
   *   scala> val strlenAdd5 = curryLen("five!")
   *   scala> val eleven = strlenAdd5("sixSIX")
   *   eleven: Int = 11
   *
   *   scala> val strlen2Uncurried = uncurry(curryLen)
   *   scala> val six = strlen2Uncurried("one", "two")
   *   six: Int = 6
   *
   *   -OR-
   *   scala> cal strlen2Uncurried = Function.uncurried(curryLen)
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  /**  scala> val f = (x: Double) => math.Pi / 2 - x
   *   scala> val cos = f andThen math.sin
   *   scala> val cosF = math.sin _ compose f
   *   scala> val cosMan = compose(math.sin, f)
   *
   *   scala> cos(15)
   *   res1: Double = -0.7596879128588213
   *
   *   scala> cosMan(15)
   *   res2: Double = -0.7596879128588213
   *
   *   scala> cosF(15)
   *   res3: Double = -0.7596879128588213
   *
   *   scala> math.cos(15)
   *   res4: Double = -0.7596879128588213
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}