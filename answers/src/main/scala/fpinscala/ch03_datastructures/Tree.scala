package fpinscala.ch03_datastructures


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.26

  def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
  }

  /* The implementation above is naive in that it is non-tail recursive and can
   * overflow the stack.  In previous exercises for 'List' we explicitly
   * spelled out variant implementations using techniques like internal
   * mutation and CPS.
   *
   * Internal mutation requires techniques familiar to many people with
   * traditional imperative languages.  To keep the focus on referential
   * transparency, we'll focus on this point forward on purely functional
   * implementations.
   *
   * Similarly, CPS is a route transformation; once you are experienced with
   * the transformation, it's fairly easy to apply to all algorithms.  Also,
   * CPS is not as broadly used as other techniques.
   *
   * Moving forward, we'll present only the simple solutions unless asked
   * otherwise by the exercise.  But feel free to experiment with different
   * implementations to develop your skill and insight.
   */

  // Exercise 3.27

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
  }

  /* We are using the method `max` that exists on all `Int` values rather than
   * an explicit `if` expression.
   *
   * Notice how similar the implementation is to `size`.  We'll abstract out
   * the common pattern in a later exercise.
   */

  // Exercise 3.28

  def depth[A](t: Tree[A]): Int = t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  // Exericse 3.29

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exericse 3.30a

  def fold[A,B]
      (tree: Tree[A])
      (map: A=>B)
      (reduce: (B, B) => B)
      : B =
    tree match {
      case Leaf(v) => map(v)
      case Branch(l, r) => reduce(fold(l)(map)(reduce), fold(r)(map)(reduce))
    }

  def assertFoldCatamorphism() {

    val tree =
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(3), Leaf(4)))

    val identityCatamorphism =
        fold(tree) { Leaf(_): Tree[Int] } { Branch(_, _): Tree[Int] }

    assert(identityCatamorphism == tree)

  }

  /* Like `foldRight` for lists, `fold` receives a "handler" for each of the
   * data constructors of the type, and recursively accumulates some value
   * using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_))
   * == t`, and we can use this function to implement just about any recursive
   * function that would otherwise be defined by pattern matching.
   *
   * This generalization of "folding" across arbitrary algebraic datatypes has
   * been formalized and is referred to by the term "catamorphism."
   * Catamorphisms have a number of laws, one of which is that we can obtain an
   * identity function by using constructors of the algebra.
   */

  object UsingFold {

    // Exericse 3.30b

    def size[A](t: Tree[A]): Int = {
      fold(t) { _ => 1} { 1 + _ + _ }
    }

    // Exericse 3.30c

    def maximum(t: Tree[Int]): Int =
      fold(t)(identity) { _ max _ }

    // Exericse 3.30d

    def depth[A](t: Tree[A]): Int =
      fold(t) { _ => 1 } { 1 + _ max _ }

    // Exericse 3.30e

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t) { v => Leaf(f(v)): Tree[B] } { Branch(_, _) }

  /* Notice the type annotation required on the expression `Leaf(f(a))`.
   * Without this annotation, we get an error like this:
   *
   *     type mismatch;
   *       found   : fpinscala.datastructures.Branch[B]
   *       required: fpinscala.datastructures.Leaf[B]
   *          fold(t)(a => Leaf(f(a)))(Branch(_,_))
   *                                  ^
   * This error is an unfortunate consequence of Scala using subtyping to
   * encode algebraic data types. Without the annotation, the result type of
   * the fold gets inferred as `Leaf[B]` and it is then expected that the
   * second argument to `fold` will return `Leaf[B]`, which it does not (it
   * returns `Branch[B]`). Really, we would prefer if Scala would infer
   * `Tree[B]` as the result type in both cases. When working with algebraic
   * data types in Scala, it is somewhat common to define helper functions that
   * simply call the corresponding data constructors but give the less specific
   * result type:
   *
   *     def leaf[A](a: A): Tree[A] = Leaf(a)
   *
   *     def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
   */

  /* Finally, notice that all of the previous implementations of 'size',
   * 'maximum', 'depth', and 'map' have suffered from potential stack overflows
   * for large enough trees.  By extracting recursion to a generalized function
   * like 'fold' we can fix the problem just in the 'fold' (say with a
   * technique like internal mutation) and all of our implementations based on
   * 'fold' benefit from the improvement.
   */

  }

}
