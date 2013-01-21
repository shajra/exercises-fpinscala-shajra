package fpinscala.ch04_errorhandling


sealed trait Option[+A] {

  // Exercise 4.1

  /* We'll first implement everything completely with pattern matching. */

  def map[B](f: A => B): Option[B] =
    ???

  def getOrElse[B >: A](default: => B): B =
    ???

  def flatMap[B](f: A => Option[B]): Option[B] =
    ???

  def orElse[B >: A](default: => Option[B]): Option[B] =
    ???

  def filter(f: A => Boolean): Option[A] =
    ???

}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2

  def variance(xs: Seq[Double]): Option[Double] =
    ???

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

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    ???

  // Exercise 4.4

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    ???

  // Exercise 4.5

  object Sequence {

    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      ???

  }

  // Exercise 4.6

  object Traverse {

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      ???

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      ???

  }

}
