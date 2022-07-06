package com.progfun.worksheet

object TestPattern {

  case class A(p1: Int, p2: Int)

  case class B(p1: String, p2: String)

  def foo(msg: Any) = msg match {
    case A(_, 2) | B(_, _) => println("ok")
  }

  def boo(msg: Any) = msg match {
    case _: A | _: B => println("ok")
  }

  def goo(msg: Any) = msg match {
    case _: A | B => println("ok")
  }

  def main(args: Array[String]) {
    foo(A(1, 2))
    boo(A(1, 2))
    goo(A(1, 2))
    //goo(B(1, 2))
    goo(B)
  }
}
