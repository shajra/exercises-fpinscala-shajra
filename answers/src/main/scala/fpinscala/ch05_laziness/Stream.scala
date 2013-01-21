package fpinscala.ch05_laziness


import Stream._
import annotation.tailrec


trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false) { (a, b) => p(a) || b }

  // Exercise 5.1

  object ToList {

    object MutatingInternally {

      def toList: List[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        @tailrec
        def build(s: Stream[A]): List[A] = s uncons match {
          case Some((h,t)) => buf += h; build(t)
          case _ => buf.toList
        }
        build(Stream.this)
      }

    }

    object Recursive {

      def toList: List[A] = uncons match {
        case Some((h,t)) => h :: t.toList
        case _ => List()
      }

    }

    object UsingFoldRight {

      def toList: List[A] = foldRight[List[A]](Nil)(_ :: _)

    }

  }

  /* We'll use toList to easily print streams in the REPL.  To make clear that
   * stack overflows are not caused by toList, we'll choose the safer
   * implementation
   */

  def toList = ToList.MutatingInternally.toList

  object SimpleTakes {

    // Exercise 5.2

    def take(n: Int): Stream[A] = uncons match {
      case Some((h, t)) if n > 0 => cons(h, t take (n - 1))
      case _ => empty
    }

    // Exercise 5.3

    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case Some((h, t)) if p(h) => cons(h, t takeWhile p)
      case _ => empty
    }

  }

  // Exercise 5.4

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => p(a) && b }

  /* Since `&&` is non-strict in its second argument, this terminates the
   * traversal as soon as a nonmatching element is found.
   */

  object UsingFoldRight {

    // Exercise 5.5

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A]) { (a, acc) =>
        if (p(a)) cons(a, acc)
        else empty
      }

    // Exercise 5.6.a

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B]) { (a, acc) => cons(f(a), acc) }

  }

  // Exercise 5.6.b

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, acc) =>
      if (p(a)) cons(a, acc)
      else acc
    }

  // Exercise 5.6.c

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s) { (a, acc) => cons(a, acc) }

  /* Perhaps you tried to write 'append' with a Stream[A] parameter and got
   * the following error:
   *
   *     covariant type A occurs in contravariant position in type
   *     fpinscala.laziness.Stream[A] of value s
   *
   *         def append(s: Stream[A]): Stream[A] =
   *                    ^
   *     one error found
   *
   * Notice we didn't have this problem when we defined 'append' for List on
   * the List companion object rather than as a method on the List trait.
   *
   * Variance introduces some complications because we have to address the
   * general case in which methods are written for specific abstractions.
   *
   * Imagine if we defined:
   *
   *     class FooStream extends Stream[Foo] {
   *         @override
   *         def append(s: Stream[Foo]): Stream[Foo] = ...
   *     }
   *
   * and specialized the implementation for only Foos and not Objects, Stream
   * could no longer be used covariantly:
   *
   *     (FooStream:Stream[Object]).append(objectStream)
   *
   * But our implementation for 'append' doesn't have this kind of
   * specialized implementation. To help the compiler understand this, we
   * introduce another type B as a super type of A, as we have in the above
   * implementation of 'append'.
   */

  // Exercise 5.6.d

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { f(_) append _ }

  // Exercise 5.12.a

  /* `uncons` has exactly the correct signature needed for `unfold`.  This is
   * analogous to the observation in Exercise 3.9 where we noticed that
   * foldRight, when given Nil and { Cons(_, _) } as arguments yields an
   * identity transformation.  We had a similar observation for Tree in
   * Exercise 3.30 when passing fold { Leaf(_) } and { Branch(_, _) } as
   * arguments.
   *
   * To better understand 'unfold', Consider the following assertion, which
   * shows how we can make an identity transformation by passing in { _.uncons
   * } as an argument:
   */

  def unfoldUnconsAssert() {
    val someStream = Stream(1, 2, 3)
    val evaluated = unfold(someStream) { _.uncons }
    assert(evaluated == someStream)
  }

  /* To implement `Stream.map`, we simply transform the first element of the
   * pair returned, using the `map` method on `Option` (discussed in chapter
   * 4).
   */

  def map[B](f: A => B): Stream[B] =
    unfold(this) { _.uncons map { case (h, t) => (f(h), t) } }

  /* Scala provides shorter syntax when the first action of a function literal
   * is to match on an expression.  The anonymous function used above in 'map'
   *
   *     { case (h, t) => {f(h), t) }
   *
   * is equivalent to
   *
   *     { p => p match { case (h, t) => (f(h), t) }
   *
   * but we avoid having to choose a name for `p`, only to pattern match on it.
   * Additionally, this shorter syntax supports multiple case blocks.
   */

  // Exercise 5.12.b

  def take(n: Int): Stream[A] =
    unfold((this, n)) { case (s, n) =>
      for ((h, t) <- s.uncons if n > 0) yield (h, (t, n - 1))
    }

  // Exericse 5.12.c

  object TakeWhile {

    object PatternMatching {

      def takeWhile(p: A => Boolean): Stream[A] =
        unfold(Stream.this) { s =>
          s.uncons match {
            case s@Some((h,_)) if p(h) => s
            case _ => None
          }
        }

      /* Notice we are using a _pattern label_ here.  In front of any pattern,
       * `x`, we can say `labelName@x`, to introduce a variable we can
       * reference on the right-hand side.  Here we are giving `Some((h,_))` a
       * name to allow us to simply reuse the value (rather than reconstructing
       * it) if the guard (in this case `if p(h)`) passes.
       */

    }

    object ForComprehension {

      /* For comprehensions can be rather elegant once mastered. */

      def takeWhile(p: A => Boolean): Stream[A] =
        unfold(Stream.this) { s =>
          for ((h, t) <- s.uncons if p(h)) yield (h, t)
        }

    }

  }

  def takeWhile(p: A => Boolean): Stream[A] =
    TakeWhile.ForComprehension.takeWhile(p)

  // Exericse 5.12.d

  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    unfold((this, bs)) { case (_as, _bs) =>
      for {
        (ah, at) <- _as.uncons
        (bh, bt) <- _bs.uncons
      } yield ((ah, bh), (at, bt))
    }

  // Exericse 5.12.e

  /* For the sake of illustrating a deeper abstraction, we've pulled out a more
   * general 'zipWithAll' from 'zipAll'.
   */

  def zipWithAll[B,C]
      (bs: Stream[B])
      (f: (Option[A], Option[B]) => C)
      : Stream[C] = {
    def unsequence[D](streamOpt: Option[Stream[D]]) =
      streamOpt.flatMap { _ uncons }
        .map { case (ah, at) => (Some(ah), Some(at)) }
        .getOrElse((None, None))
    unfold((Option(this), Option(bs))) { case (asOpt, bsOpt) =>
      val (ah, at) = unsequence(asOpt)
      val (bh, bt) = unsequence(bsOpt)
      Some((f(ah, bh), (at, bt)))
    }
  }

  /* Notice how this implementation of 'zipWithAll' uses flatMap, map, and
   * getOrElse on Option to avoid pattern matching and calls to Option#get.
   *
   * Also note that we use Option.apply in the first parameter of the unfold
   * call rather than calling the Some constructor.  The standard library
   * offers this to help users avoid casting Some to the more general Option.
   */

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(bs) { (_,_) }

  // Exercise 5.14

  def tails: Stream[Stream[A]] = {
    val tailFor = { (s:Stream[A]) => s.uncons.map { case (h, t) => (s, t) } }
    unfold(this)(tailFor) append Stream(empty)
  }

  /* The last element of `tails` is always the empty `Stream`, so we handle
   * this as a special case, by appending it to the output.
   */

  // Exercise 5.15

  /* 'scanRight can't be implemented using `unfold`, since `unfold` generates
   * elements of the `Stream` from left to right. It can be implemented using
   * `foldRight` though.
   */

  def scanRight[B](b: B)(f: (A, B) => B): Stream[B] =
    foldRight((b, Stream(b))) { case (a, (b, acc)) =>
      val next = f(a, b)
      (next, cons(next, acc))
    } _2

  object UsingScanRight {

    def tails: Stream[Stream[A]] =
      scanRight(empty[A]) { cons(_, _) }

  }

  /* The implementation is just a `foldRight` that keeps the accumulated value
   * and the stream of intermediate results, which we `cons` onto during each
   * iteration.  When writing folds, it's common to have more state in the fold
   * than is needed to compute the result.  Here, we simply extract the
   * accumulated list once finished.
   */

}


object Stream {

  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] =
    cons(1, ones)

  // Exercise 5.7

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // Exercise 5.8

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // Exercise 5.9

  def fibs: Stream[Int] = {
    def fibs(prev: Int, prev2: Int): Stream[Int] = {
      val next = prev + prev2
      cons(next, fibs(next, prev))
    }
    cons(0, cons(1, fibs(1, 0)))
  }

  // Exercise 5.10

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) match {
      case Some((nextA, nextS)) => cons(nextA, unfold(nextS)(f))
      case None => empty
    }

  object UsingUnfold {

    // Exercise 5.11.a

    def fibs: Stream[Int] =
      unfold((1,0)) { prevs =>
        val next = prevs._1 + prevs._2
        Some((prevs._2, (next, prevs._1)))
      }

    // Exercise 5.11.b

    def from(n: Int): Stream[Int] =
      unfold(n) { n => Some((n, n+1)) }

    // Exercise 5.11.c

    def constant[A](a: A): Stream[A] =
      unfold(a) { a => Some((a,a)) }

    // Exercise 5.11.d

    def ones: Stream[Int] =
      unfold(1) { _ => Some((1,1)) }

  }

  // Exercise 5.13

  def startsWith[A](as: Stream[A], prefix: Stream[A]): Boolean = {
    val hasPrefix = { (a: Option[A], p: Option[A]) => ! p.isEmpty }.tupled
    val prefixMatches: ((Option[A], Option[A])) => Boolean = {
        case (Some(a), Some(p)) if a == p => true
        case _ => false
    }
    as zipAll prefix takeWhile hasPrefix forAll prefixMatches
  }

  /* Notice that it is possible to factor out first class functions to give
   * them names if that helps the reader follow the algorithm.  However, this
   * comes with the cost of providing more type annotations to satisfy the
   * compiler.
   */

}
