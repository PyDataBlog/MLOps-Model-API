package object tests {
  trait Matcher[X <: AnyRef]{}
  
  object Problem2{
    def allOf[X](x: Matcher[_ >: X], y: Matcher[_ >: X]) = new Matcher[X](){}
    def allOf[X](x: Matcher[_ >: X]*) = new Matcher[X](){}
  }
}
