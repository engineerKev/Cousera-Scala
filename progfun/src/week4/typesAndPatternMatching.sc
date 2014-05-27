package week4
import week03._

object typesAndPatternMatching {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }

  //class Nil[T] extends List[T] {
  object Nil extends List[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }

  /*object List {
    List(1,2) = List.apply(1,2)
    def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
    def apply[T]() = new Nil
    def apply[T](x1:T): List[T] = new Cons(x1,new Nil)
  }*/

  object test {
    val x: List[String] = Nil
    def f(xs: List[Nothing], x: Nothing) = xs prepend x
  }

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor = new Succ(this)
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor = throw new Error("0.pred")
    def +(that: Nat) = that
    def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
  }

  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def +(that: Nat) = new Succ(n + that)
    def -(that: Nat) = if (that.isZero) this else n - that.predecessor
  }
}