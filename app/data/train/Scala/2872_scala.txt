package week4

import java.util.NoSuchElementException

import week3.{Empty, IntSet, NonEmpty}

trait List[+T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
  def nth(n: Int): T
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def nth(n: Int): T = {
    def loop(i: Int, list: List[T]): T = {
      if (list.isEmpty) throw new IndexOutOfBoundsException("index out of bound: " + n)
      else if (i == n) list.head
      else loop(i + 1, list.tail)
    }

    loop(0, this)
  }
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def nth(n: Int): Nothing = throw new NoSuchElementException("Nil.nth(" + n + ")")
}

object List {
  def singleton(elem: Any) = new Cons(elem, Nil)
  def apply[T](x1: T, x2:T): List[T] = new Cons(x1, new Cons(x2, Nil))
  def apply(): List[Nothing] = Nil
  val x: List[String] = Nil

  def f(xs:List[NonEmpty]):List[IntSet] = xs prepend Empty
}

