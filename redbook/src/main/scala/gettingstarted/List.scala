package gettingstarted

sealed trait List[+A] // the + is a variance annotation (omitting would make this invariant for A, any)
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { //companion of built-in List
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs) // lispy lisp!
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax, that is, zero or more params
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  /** 3.1
   *
   * this will return 3 (because 1+2=3)
   */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //this one will match!
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /** scala> List.append(List(1,2,3), List(4,5))
   * res0: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  /**
   *
   * NOTE: not tail recursive, will result in a giant heap!
   */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /** 3.2
   *
   * scala> List.tail(List(1,2,3,4,5))
   * res0: List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,Nil))))
   */
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }


  /** 3.3
   *
   * scala> List.setHead(List(2,3,4,5), 1)
   * res0: List[Int] = Cons(1,Cons(3,Cons(4,Cons(5,Nil))))
   */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }


  /** 3.4
   *
   * scala> List.drop(List(1,2,3,4,5), 2)
   * res0: List[Int] = Cons(3,Cons(4,Cons(5,Nil)))
   */
  def drop[A](list: List[A], n: Int): List[A] =
    if (n <= 0) list
    else list match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }


  /** 3.5
   *
   * scala> List.dropWhile(List(1,2,3,4,5), (x: Int) => {x <= 3})
   * res0: List[Int] = Cons(4,Cons(5,Nil))
   */
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] =
    list match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => list
    }


  /** 3.6
   *
   * scala> List.init( List(1,2,3,4) )
   * res0: List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
   */
  def init[A](list: List[A]): List[A] =
    list match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  // from answers... assembles a new list within a mutable ListBuffer
  def init2[A](list: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(list)
  }


  /** 3.7
   *
   *  Not possible, foldRight evaluates the entire list
  */


  /** 3.8
   *
   * scala> List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
   * res0: List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
   *
   * it's the original list!
   */


  /** 3.9
   *
   * scala> List.length(List(1,2,3))
   * res0: Int = 3
   *
   */
  def length[A](list: List[A]): Int =
    foldRight(list, 0)((_,acc) => acc + 1)


  /** 3.10
   *
   * scala> List.foldLeft(List(1,2,3), 1)((x: Int, y: Int) => (x+y))
   * res0: Int = 7
   *
   */
  @annotation.tailrec
  def foldLeft[A,B](list: List[A], z: B)(f: (B, A) => B): B =
    list match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }


  /** 3.11
   *
   * scala> List.sum3(List(1,2,3))
   * res0: Int = 6
   *
   * scala> List.product3(List(1,2,3))
   * res1: Double = 6.0
   *
   * scala> List.length2(List(1,2,3))
   * res2: Int = 3
   *
   */
  def sum3(list: List[Int]) = foldLeft(list, 0)(_ + _)
  def product3(list: List[Double]) = foldLeft(list, 1.0)(_ * _)
  def length2[A](list: List[A]): Int = foldLeft(list, 0)((acc,h) => acc + 1)


  /** 3.12
   *
   * scala> List.reverse(List(1,2,3))
   * res0: List[Int] = Cons(3,Cons(2,Cons(1,Nil)))
   *
   */
  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]())((acc,h) => Cons(h,acc))


  /** 3.13
   *
   * scala> List.foldRightViaFoldLeft(List(1,2,3), 1)((x: Int, y: Int) => (x+y))
   * res0: Int = 7
   *
   */
  def foldRightViaFoldLeft[A,B](list: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(list), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](list: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(list, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](list: List[A], z: B)(f: (B,A) => B): B =
    foldRight(list, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  /** 3.14
   *
   * scala> List.append2(List(1,2,3), List(4,5,6))
   * res0: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))
   *
   */
  def append2[A](list1: List[A], list2: List[A]): List[A] =
    foldRight(list1, list2)(Cons(_,_))

  def append2left[A](list1: List[A], list2: List[A]): List[A] =
    foldLeft(reverse(list1), list2)((x,y) => Cons(y,x))


  /** 3.15
   *
   * scala> List.concat(List(List(1,2,3),List(4,5,6)))
   * res0: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))
   *
   * This is effectively a flatMap with identity function
   */
  def concat[A](list: List[List[A]]): List[A] =
    foldRight(list, Nil:List[A])(append)


  /** 3.16
   *
   * scala> List.add1(List(1,2,3))
   * res0: List[Int] = Cons(2,Cons(3,Cons(4,Nil))))
   *
   */
  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil:List[Int])((h,t) => Cons(h+1,t))

  def add1fl(list: List[Int]): List[Int] =
    foldLeft(list, Nil:List[Int])((t,h) => append(t,List(h+1)))


/** 3.17
 *
 * scala> List.doubleToString(List(1.0,2,3))
 * res0: List[String] = Cons(1.0,Cons(2.0,Cons(3.0,Nil)))
 */
  def doubleToString(list: List[Double]): List[String] =
    foldRight(list, Nil:List[String])((h,t) => Cons(h.toString,t))

  def doubleToStringfl(list: List[Double]): List[String] =
    foldLeft(list, Nil:List[String])((t,h) => append(t, List(h.toString)))


  /** 3.18
   *
   * scala> List.map(List(1,2,3))(_+1.5)
   * res0: List[Double] = Cons(2.5,Cons(3.5,Cons(4.5,Nil)))
   */
  def map[A,B](list: List[A])(f: A => B): List[B] =
    foldRight(list, Nil:List[B])((h,t) => Cons(f(h),t))

  def mapfl[A,B](list: List[A])(f: A => B): List[B] =
    foldLeft(list, Nil:List[B])((t,h) => append(t, List(f(h))))

  // from answer key:
  def map_2[A,B](list: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(x: List[A]): Unit = x match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(list)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }


  /** 3.19
   *
   * scala> List.filter(List(1,2,3,4))(x => x % 2 == 0)
   * res0: List[Int] = Cons(2,Cons(4,Nil))
   */
  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    foldRight(list, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def filterfl[A](list: List[A])(f: A => Boolean): List[A] =
    foldLeft(list, Nil:List[A])((t,h) => if (f(h)) append(t,List(h)) else t)

  // from answer key:
  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }


  /** 3.20
   *
   * scala> List.flatMap(List(1,2,3))(i => List(i,i))
   * res0: List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))
   *
   * remember that "concat" above will flatten a list of lists
   */
  def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] =
    concat(map(list)(f))


  /** 3.21
   *
   * scala> List.filterViaFlatMap(List(1,2,3,4))(x => x % 2 == 0)
   * res0: List[Int] = Cons(2,Cons(4,Nil))
   *
   */
  def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] =
    flatMap(list)(a => if (f(a)) List(a) else Nil)


  /** 3.22
   *
   * scala> List.zipSum(List(1,2,3), List(4,5,6))
   * res0: List[Int] = Cons(5,Cons(7,Cons(9,Nil)))
   *
   */
  def zipSum(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, zipSum(t1,t2))
  }


  /** 3.23
   *
   * scala> List.zipWith(List(1,2,3), List(4,5,6))(_+_)
   * res0: List[Int] = Cons(5,Cons(7,Cons(9,Nil)))
   *
   */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  /** 3.24
   *
   * scala> List.hasSubsequence(List(1,2,3,4), List(2,3))
   * res0: Boolean = true
   *
   * this naive string search that will perform poorly.
   * Consider a DAWG or Trie data structure (or even a hashmap or Set of all possible combinations in sup)
   * There's also KMP algorithm, and all kinds of w-lookahead algorithms that can speed things up
   */

  //from answer key
  @annotation.tailrec
  def startsWith[A](list: List[A], prefix: List[A]): Boolean = (list,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t), Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}