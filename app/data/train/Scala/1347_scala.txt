package technology.walrath.euler

/**
 * Created by justin on 2/13/15.
 */
object E25 {
  def main(args: Array[String]): Unit = {
    lazy val f: Stream[Int] = 0 #:: f.scanLeft(1)(_ + _)
    println(f.takeWhile(_.toString.length < 1000).size)
  }
}
