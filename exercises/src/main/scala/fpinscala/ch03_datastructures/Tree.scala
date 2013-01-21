package fpinscala.ch03_datastructures


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.26

  def size[A](t: Tree[A]): Int =
    ???

  // Exercise 3.27

  def maximum(t: Tree[Int]): Int =
    ???

  // Exercise 3.28

  def depth[A](t: Tree[A]): Int =
    ???

  // Exericse 3.29

  def map /* try designing the signature yourself */ =
    ???

  // Exericse 3.30a

  def fold /* try designing the signature yourself */ =
    ???

  object UsingFold {

    // Exericse 3.30b

    def size[A](t: Tree[A]): Int =
      ???

    // Exericse 3.30c

    def maximum(t: Tree[Int]): Int =
      ???

    // Exericse 3.30d

    def depth[A](t: Tree[A]): Int =
      ???

    // Exericse 3.30e

    def map /* use the same signature from Exercise 3.29 */ =
      ???

  }

}
