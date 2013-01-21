package fpinscala.ch04_errorhandling


sealed trait Option[+A] {

  // Exercise 4.1

  /* We'll first implement everything completely with pattern matching. */

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => default
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  object ReusingFunctions {

    /* Now we'll reuse some functions to define others. */

    def flatMap[B](f: A => Option[B]): Option[B] =
      Option.this map f getOrElse None

    def orElse[B >: A](default: => Option[B]): Option[B] =
      Option.this map { Some(_) } getOrElse default

    def filter(f: A => Boolean): Option[A] =
      Option.this flatMap { a => if (f(a)) Some(a) else None }

  }

}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m => mean(xs map { x => math.pow(x - m, 2) }) }
  }

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    for { p <- pattern(pat) } yield { p.matcher(_).matches }

  // Exercise 4.3

  object Map2 {

    /* The for-comprehension syntax is somewhat clearer.  Here are both
     * versions.
     */

    object WithForComprehension {

      def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        for (_a <- a; _b <- b) yield f(_a, _b)

    }

    object Desugared {

      def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a flatMap { _a =>
          b map { _b =>
            f(_a, _b)
          }
        }

    }

  }

  /* choosing an implementation for later use */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    Map2.WithForComprehension.map2(a, b)(f)

  // Exercise 4.4

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2)) { (f1, f2) => f1(s) && f2(s) }

  // Exercise 4.5

  object Sequence {

    object PatternMatching {

      def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
        case Nil => Some(Nil)
        case h :: t => h flatMap { a => sequence(t) map { a :: _ } }
      }

    }

    object UsingListFoldRight {

      def sequence[A](as: List[Option[A]]): Option[List[A]] =
        as.foldRight[Option[List[A]]](Some(Nil)) { (a, acc) =>
          map2(a, acc) { _ :: _ }
        }

    }

  }

  // Exercise 4.6

  object Traverse {

    object PatternMatching {

      def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
        as match {
          case Nil => Some(Nil)
          case h :: t => map2(f(h), traverse(t)(f)) { _ :: _ }
        }

    }

    object UsingListFoldRight {

      def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
        as.foldRight[Option[List[B]]](Some(Nil)) { (a, acc) =>
          map2(f(a), acc) { _ :: _ }
        }

      /* The type annotation on `foldRight` is needed here, otherwise Scala
       * wrongly infers the result type of the fold as `Some[Nil.type]` and
       * reports a type error (try it!).  This is an unfortunate consequence of
       * Scala using subtyping to encode algebraic data types.
       */

    }

    /* choosing an implementation */
    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      UsingListFoldRight.traverse(as)(f)

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(identity)

  }

}
