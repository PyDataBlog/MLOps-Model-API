import org.apache.spark._

object Main {
  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("test").setMaster("local[*]")
    val sc = new SparkContext(conf)

    val values1 = sc.parallelize(Seq("ahmad","amr","tamer","ali"))
    val values2 = sc.parallelize(Seq("ali","tamer","alaa"))
    values1.intersection(values2).collect
  
  }
}
