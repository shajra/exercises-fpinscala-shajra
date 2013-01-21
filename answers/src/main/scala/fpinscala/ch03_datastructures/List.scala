package fpinscala.ch03_datastructures


import annotation.tailrec
import sys.error


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  val cascadingMatch = List(1,2,3,4,5) match {

    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42

    // this is the match with 'x' bound to 1 and 'y' bound to 2
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y

    case Cons(h, t) => h + sum(t)
    case _ => 101

  }

  def cascadingMatchAssert() {
    val answer = 3
    assert(cascadingMatch == answer)
  }

  // Exercise 3.2

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => error("no tail of empty list")
    case Cons(_, t) => t
  }

  /* Although we could return `Nil` when the input list is empty, we choose to
   * throw an exception instead.  This is a somewhat subjective choice.  In our
   * experience taking the tail of an empty list is often a bug, and silently
   * returning a value just means this bug will be discovered later, further
   * from the place where it was introduced.
   */

  /* It's generally good practice when pattern matching to use '_' for any
   * variables you don't intend to use on the right hand side of a pattern.  It
   * makes it clear the value isn't relevant.
   */

  // Exercise 3.3

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n - 1)
    }

  /* Again, it is somewhat subjective whether to throw an exception when asked
   * to drop more elements than the list contains.  The usual default for
   * `drop` is not to throw an exception, since it is typically used in cases
   * where this is not indicative of a programming error.  If you pay attention
   * to how you use `drop`, it is often in cases where the length of the input
   * list is unknown, and the number of elements to be dropped is being
   * computed from something else.  If `drop` threw an exception, we'd have to
   * first compute or check the length and only drop up to that many elements.
   */

  // Exercise 3.4

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

  /* Somewhat overkill, but to illustrate the feature we are using a _pattern
   * guard_, to only match a `Cons` whose head satisfies our predicate, `f`.
   * The syntax is simply to add `if <cond>` after the pattern, before the
   * `=>`, where `<cond>` can use any of the variables introduced by the
   * pattern.
   */

  // Exercise 3.5

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Nil => error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /* If a function body consists solely of a match expression, we'll often put
   * the match on the same line as the function signature, rather than
   * introducing another level of nesting.
   */

  // Exercise 3.6

  object Init {

    object Naive {

      def init[A](l: List[A]): List[A] = l match {
        case Nil => error("no init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }

      /* Notice we are copying the entire list up until the last element.
       * Besides being inefficient, the natural recursive solution will use a
       * stack frame for each element of the list, which can lead to stack
       * overflows for large lists (can you see why?).
       */

    }

    object MutatingInternally {

      /* With lists, it's common to use a temporary, mutable buffer internal to
       * the function (with lazy lists or streams which we discuss in chapter
       * 5, we don't normally do this).  So long as the buffer is allocated
       * internal to the function, the mutation is not observable and RT is
       * preserved.
       */

      def init[A](l: List[A]): List[A] = {
        import collection.mutable.ListBuffer
        val buf = new ListBuffer[A]
        @tailrec
        def build(cur: List[A]): List[A] = cur match {
          case Nil => sys.error("init of empty list")
          case Cons(_,Nil) => List(buf.toList: _*)
          case Cons(h,t) => buf += h; build(t)
        }
        build(l)
      }

    }

    object Cps {

      /* Continuation passing style (CPS) is interesting to practice because it
       * maintains much of the form of the naive solution.  In this case of
       * init, we can also obtain a tail recursion simple enough for the Scala
       * compiler to optimize.
       *
       * One compelling reason to choose internal mutation over CPS in this
       * instance is performance, though the difference is probably too minute
       * to worry about for most usages.
       */

      def init[A](l: List[A]): List[A] = {
        @tailrec
        def initCps[A](l: List[A])(cont: List[A] => List[A]): List[A] =
          l match {
            case Nil => error("no init of empty list")
            case Cons(_, Nil) => cont(Nil)
            case Cons(h, t) => initCps(t) { rest => cont(Cons(h, rest)) }
          }
        initCps(l)(identity)
      }

      /* Note that we're using the 'identity' method that the Scala library
       * puts in scope by default.  This is less verbose than (x => x).
       *
       * 'identity' is defined as:
       *
       *     def identity[A](x: A): A = x
       *
       * In Scala there is a rather arbitrary distinction between functions
       * defined as _methods_, which are introduced with the `def` keyword, and
       * function values, which are the first-class objects we can pass to
       * other functions, put in collections, and so on.  Converting a method
       * to a first-class function is called eta expansion and in Scala is
       * accomplished by suffixing the method name with an underscore, which in
       * the method above would look like:
       *
       *     initCps(l)(identity _)
       *
       * However, in some cases (including our CPS 'init' above) Scala does the
       * eta expansion automatically and we can pretend there is no distinction
       * between methods and first-class functions.
       */

    }

    /* Another common convention is to accumulate the output list in reverse
     * order, then reverse it at the end, which does not require even local
     * mutation. We will write a reverse function later in this chapter.
     */

  }

  // Exercise 3.7

  /* Evaluation of foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _):
   *
   *     foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
   *     1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
   *     1 + (2 + foldRight(Cons(3, Nil), 0)(_ + _))
   *     1 + (2 + (3 + (foldRight(Nil, 0)(_ + _))))
   *     1 + (2 + (3 + (0)))
   *     1 + (2 + (3))
   *     1 + (5)
   *     6
   *
   * Notice that we traverse all the way to the end of the list before we can
   * begin collapsing it.  For a simple recursive implementation, this will
   * push frames onto the call stack as we go, which can overflow the stack for
   * large enough lists.
   */

  // Exercise 3.8

  /* No, short-circuiting with a foldRight invocation is not possible! The
   * reason is that _before_ we ever call our function, `f`, we evaluate its
   * argument, which in the case of `foldRight` means traversing the list all
   * the way to the end.  We need _non-strict_ evaluation to support early
   * termination -- we discuss this in chapter 5.
   */

  // Exercise 3.9

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightConsAssert() {
    val someList = List(1, 2, 3)
    val evaluated = foldRight(someList, Nil: List[Int]) { Cons(_, _) }
    val answer = someList
    assert(evaluated == answer)
  }

  /* We get back the original list!  Why is that?  As we mentioned earlier, one
   * way of thinking about what `foldRight` "does" is it replaces the `Nil`
   * constructor of the list with the `z` argument, and it replaces the `Cons`
   * constructor with the given function, `f`.  If we just supply `Nil` for `z`
   * and `Cons` for `f`, then we get back the input list:
   *
   *     foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
   *     Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
   *     Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
   *     Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
   *     Cons(1, Cons(2, Cons(3, Nil)))
   */


  // Exercise 3.10

  def length[A](l: List[A]): Int =
    foldRight(l, 0) { (_, acc) => acc + 1 }

  // Exercise 3.11

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // Exercise 3.12

  object AggregatesUsingFoldLeft {

    def sum(ints: List[Int]) =
      foldLeft(ints, 0.0)(_ + _)

    def product(ints: List[Double]) =
      foldLeft(ints, 1.0)(_ * _)

    def length[A](l: List[A]): Int =
      foldLeft(l, 0) { (acc, _) => acc + 1 }

  }

  // Exercise 3.13

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  // Exercise 3.14

  object FoldsWithFolds {

    object UsingFunctionChaining {

      /* These implementations build up a chain of functions which, when
       * called, results in the operations being performed with the correct
       * associativity.
       */

      def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
        foldRight(l, identity[B] _){(a, cont) => b => cont(f(b,a))}(z)

      def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(l, identity[B] _){(cont, a) => b => cont(f(a,b))}(z)

      /* Notice that because identity is polymorphic and the type arguments
       * can not be inferred, we must help the compiler out by explicitly
       * specifying the type.  Also, we must perform the eta expansion
       * manually.
       *
       * Another alternative is to abandon using identity altogether and just
       * spell out (b:B) => b.
       */

    }

    object UsingReverse {

      /* This implementation of `foldRight` in terms of `reverse` and
       * `foldLeft` is a common trick for avoiding stack overflows when
       * implementing a strict `foldRight` function as we've done in this
       * chapter (we will revisit this in a later chapter, when we discuss
       * laziness).
       */

      def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(reverse(l), z)((b,a) => f(a,b))

    }

  }

  // Exercise 3.15

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r) { Cons(_, _) }

  /* `append` simply replaces the `Nil` constructor of the first list with the
   * second list, which is exactly the operation performed by `foldRight`.
   *
   * Since `append` takes time proportional to its first argument, and this
   * first argument never grows because of the right-associativity of
   * `foldRight`, this function is linear in the total length of all lists. You
   * may want to try tracing the execution of the implementation on paper to
   * convince yourself that this works.
   */

  // Exercise 3.16

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A]){ append }

  /* Notice we are simply referencing the `append` function, without writing
   * something like `(x,y) => append(x,y)` or `append(_,_)` because of Scala's
   * automatic eta expansion.  In other cases, you'll be forced to perform
   * the eta expansion manually
   *
   *     append _
   *
   * or even specify types fully
   *
   *     (x: List[A], y: List[A]) => append(x,y)
   *
   * if the function is polymorphic and the type arguments are not known.
   */

  // Exercise 3.17

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int]) { (e, acc) => Cons(e + 1, acc) }

  // Exercise 3.18

  def doubles2Strings(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String]) { (e, acc) => Cons(e.toString, acc) }

  // Exercise 3.19

  object Map {

    object Naive {

      /* A natural solution is using `foldRight`, however this will stack
       * overflow for large lists.
       */

      def map[A,B](l: List[A])(f: A => B): List[B] =
        foldRight(l, Nil: List[B]) { (a, bs) => Cons(f(a), bs) }

    }

    object UsingFoldRightUsingFoldLeft {

      /* Alternatively, we can use `foldRightViaFoldLeft` to avoid the stack
       * overflow.
       */

      import FoldsWithFolds.UsingFunctionChaining.{foldRight => safeFoldRight}

      def map[A,B](l: List[A])(f: A => B): List[B] =
        safeFoldRight(l, Nil: List[B]) { (a, bs) => Cons(f(a), bs) }

    }

    object MutatingInternally {

      /* More commonly, with our current implementation of `List`, `map` will
       * just be implemented using local mutation. Again, notice that the
       * mutation is not observable outside the function, since we are only
       * mutating a buffer that we have allocated.
       */

      def map[A,B](l: List[A])(f: A => B): List[B] = {
        val buf = new collection.mutable.ListBuffer[B]
        def build(cur: List[A]): Unit = cur match {
          case Nil => ()
          case Cons(h,t) => buf += f(h); build(t)
        }
        build(l)
        List(buf.toList: _*)
      }

    }

  }

  // select implementation for later use
  def map[A,B](l: List[A])(f: A => B): List[B] =
    Map.MutatingInternally.map(l)(f)

  // Exercise 3.20

  object Filter {

    /* The discussion about `map` from Exercise 3.19 also applies here. */

    object Naive {

      def filter[A](l: List[A])(f: A => Boolean): List[A] =
        foldRight(l, Nil: List[A]) { (a, acc) =>
          if (f(a)) Cons(a, acc) else acc
        }

    }

    object UsingFoldRightUsingFoldLeft {

      import FoldsWithFolds.UsingFunctionChaining.{foldRight => safeFoldRight}

      def filter[A](l: List[A])(f: A => Boolean): List[A] =
        safeFoldRight(l, Nil: List[A]) { (a, acc) =>
          if (f(a)) Cons(a, acc) else acc
        }

    }

    object MutatingInternally {

      def filter[A](l: List[A])(f: A => Boolean): List[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        def build(cur: List[A]): Unit = cur match {
          case Nil => ()
          case Cons(h,t) => if (f(h)) buf += h; build(t)
        }
        build(l)
        List(buf.toList: _*)
      }

    }

  }

  // Exercises 3.21

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  /* 'flatMap' could also be implemented directly using `foldRight`. */


  object FilterWithFlatMap {

    // Exercises 3.22

    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l){ a => if (f(a)) Cons(a, Nil) else Nil }

  }

  // Exercises 3.23

  object AddPairwise {

    object Naive {

      /* To match on multiple values, we can put the values into a pair and
       * match on the pair, as shown below, and the same syntax extends to
       * matching on N values (see sidebar "Pairs and tuples in Scala" for more
       * about pair and tuple objects).
       */

      def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
        (as, bs) match {
          case (Nil, _) =>
            Nil
          case (_, Nil) =>
            Nil
          case (Cons(ah, at), Cons(bh, bt)) =>
            Cons(ah + bh, addPairwise(at, bt))
        }

      /* As with 'map' and 'filter' in previous exercises, this implementation
       * is non-tail recursive and can overflow the stack.
       */

    }

    object NestedMatching {

      /* You can also (somewhat less conveniently, but a bit more efficient)
       * nest pattern matches: on the right hand side of the `=>`, simply begin
       * another `match` expression.  The inner `match` will have access to all
       * the variables introduced in the outer `match`.
       */

      def addPairwise(as: List[Int], bs: List[Int]): List[Int] = as match {
        case Cons(ah, at) =>
          bs match {
            case Cons(bh, bt) =>
              Cons(ah + bh, addPairwise(at, bt))
            case _ => Nil
          }
        case _ => Nil
      }

      /* Though slightly faster, this implementation is still non-tail
       * recursive and can overflow the stack.  Two solutions to potential
       * stack overflow are internal mutation and CPS, both illustrated below
       * for 'addPairwise'.
       */

    }

    object MutatingInternally {

      def addPairwise(as: List[Int], bs: List[Int]): List[Int] = {
        val buf = new collection.mutable.ListBuffer[Int]
        def build(curAs: List[Int], curBs: List[Int]): Unit = curAs match {
          case Cons(ah, at) =>
            curBs match {
              case Cons(bh, bt) =>
                buf += ah + bh; build(at, bt)
              case _ => Nil
            }
          case Nil =>
            return
        }
        build(as, bs)
        List(buf.toList: _*)
      }

    }

    object Cps {

      def addPairwise(as: List[Int], bs: List[Int]): List[Int] = {
        @tailrec
        def addPairwiseCps
            (as: List[Int], bs: List[Int])
            (cont: List[Int] => List[Int]): List[Int] = {
          as match {
            case Cons(ah, at) =>
              bs match {
                case Cons(bh, bt) =>
                  addPairwiseCps(at, bt) { rest => cont(Cons(ah + bh, rest)) }
                case _ => cont(Nil)
              }
            case _ => cont(Nil)
          }
        }
        addPairwiseCps(as, bs)(identity)
      }

    }

  }

  // Exercises 3.24

  /* This function is usually called `zipWith`.
   *
   * Previous discussions of stack usage apply here too.  The solution below is
   * a simple naive solution, but internal mutation or CPS may be used to avoid
   * stack overflow.
   *
   * By putting the `f` in the second argument list, Scala can infer its type
   * from the previous argument list.
   */

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

  // Exercise 3.25

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_,Nil) => true
    case (Nil,_) => false
    case (Cons(h,t),Cons(h2,t2)) => if (h == h2) startsWith(t, t2) else false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @tailrec
    def test[A](cur: List[A]): Boolean = cur match {
      case Nil => false
      case Cons(h,t) => if (startsWith(l, sub)) true else test(t)
    }
    test(l)
  }

  /* There's nothing particularly bad about this implementation of
   * 'hasSubsequence', except that it's somewhat monolithic and easy to get
   * wrong (without pattern matching, the implementation would be even more
   * finnicky).  Where possible, we prefer to assemble functions like this
   * using combinations of other functions.  It makes the code more obviously
   * correct and easier to read and understand.  Notice that in this
   * implementation we need special purpose logic to break out of our loops
   * early.  In Chapter 5 we'll discuss ways of composing functions like this
   * from simpler components, without giving up the efficiency of having the
   * resulting functions work in one pass over the data.
   */

}
