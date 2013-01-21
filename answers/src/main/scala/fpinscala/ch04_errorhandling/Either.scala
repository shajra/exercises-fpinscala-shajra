package fpinscala.ch04_errorhandling


sealed trait Either[+E,+A] {

 // Exercise 4.7

 def map[B](f: A => B): Either[E, B] =
   this match {
     case Left(e) => Left(e)
     case Right(a) => Right(f(a))
   }

 /* Note in 'map' that we can't simply return 'this' for the Left case because
  * it is of type Either[E,A] when we need to return Either[E,B].  Because
  * Scala encodes ADTs with subtyping, we are needlessly creating new instances
  * of Left.  We can't use simple type ascription (this: Either[E, B]) because
  * the cast is not safe from the compiler's perspective.  We can force the
  * cast with
  *
  *   this.asInstanceOf[Either[E,B]]
  *
  * but this is a bit beyond the point of this exericse.
  */

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   this match {
     case Left(e) => Left(e)
     case Right(a) => f(a)
   }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
   this match {
     case Left(e) => b
     case Right(a) => Right(a)
   }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   for (_a <- this; _b <- b) yield f(_a, _b)

}


case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]


object Either {

  // Exercise 4.8

  /* As we learned from Option, let's try implementing 'traverse' first, and
   * then make 'sequence' use 'traverse'.
   */

  object Traverse {

    /* Here's two solutions similar to those provided for Option. */

    object PatternMatching {

        def traverse[E, A, B]
            (as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
          as match {
            case Nil => Right(Nil)
            case h :: t => f(h).map2(traverse(t)(f)) { _ :: _}
          }

      }

    object UsingListFoldRight {

      def traverse[E, A, B]
          (as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        as.foldRight[Either[E, List[B]]](Right(Nil)) { (a, acc) =>
          f(a).map2(acc) { _ :: _ }
        }

    }

  }

  /* choosing an implementation */
  def traverse[E, A, B]
      (as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    Traverse.UsingListFoldRight.traverse(as)(f)

  def sequence[E, A](as: List[Either[E,A]]): Either[E, List[A]] =
    traverse(as)(identity)

  // Exercise 4.9

  /* There are a number of variations on `Option` and `Either`. If we want to
   * accumulate multiple errors, a simple approach is a new data type that lets
   * us keep a list of errors in the data constructor that represents failures:
   *
   *     trait Partial[+A,+B]
   *     case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
   *     case class Success[+B](get: B) extends Partial[Nothing,B]
   *
   * This type is called `Validation` in the Scalaz library. You can implement
   * `map`, `map2`, `sequence`, and so on for this type in such a way that
   * errors are accumulated when possible (`flatMap` is not able to accumulate
   * errors--can you see why?). This idea can even be generalized further - we
   * don't need to accumulate failing values into a list, we can accumulate
   * values using any user-supplied binary function.
   *
   * It's also possible to use `Either[List[E],_]` directly to accumulate
   * errors, using different implementations of helper functions like `map2`
   * and `sequence`.
   */

}
