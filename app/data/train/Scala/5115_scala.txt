import scala.io.Source
import scala.util.control.Breaks._


object IODemo{
    def main(args: Array[String]){
        val fileName="./test.txt"
        readAndPrint(fileName)    //读文件

        /*单个字符处理
        val source = Source.fromFile(fileName)    
        for(c <- source)
            println(c)
        */

        /* 读取url
        val source1 = Source.fromURL("http://www.baidu.com")  
        val lineIterator1 = source1.getLines()
        for(l <- lineIterator1){
            println(l)    
        }
        source1.close()
        */
         
        /*
        val source2 = Source.fromString("Hello, World!")
        val lineIterator2 = source2.getLines()
        for(l <- lineIterator2){
            println(l)    
        }
        source2.close()
        */
    
        val source3 = Source.stdin  //从标准输入读取 

        breakable{
            while(source3.hasNext){       //一个个字符判断
                val s3in = source3.next
                if(s3in == 'q'){
                    break;    
                }else{
                    println(s3in)    
                }
            }    
        }
    }




    def readAndPrint(fileName: String){
        val source = Source.fromFile(fileName)    
        val lineIterator = source.getLines()
        for(l <- lineIterator){
            println(l)    
        }
        source.close()
    }
}
