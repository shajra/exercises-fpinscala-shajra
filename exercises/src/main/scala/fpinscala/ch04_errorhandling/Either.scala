package fpinscala.ch04_errorhandling


sealed trait Either[+E,+A] {

 // Exercise 4.7

 def map[B](f: A => B): Either[E, B] =
   ???

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
   ???

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
   ???

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   ???

}


case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]


object Either {

  // Exercise 4.8

  def traverse /* try designing the signature yourself */ =
    ???

  def sequence /* try designing the signature yourself */ =
    ???

  // Exercise 4.9

  /* TODO: Explain how what kind of ADT we'd need to report on multiple errors,
   * not discarding errors beyond the first one encountered (as Either does).
   */

}
