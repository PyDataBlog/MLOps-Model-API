package com.git.huanghaifeng.basis

object Foreach {
  def main(args:Array[String]) = {  
       var greeting ="" 
       for(i <- 0 until args.length){  
           greeting += (args(i) + " ")          
       }  
       if(args.length > 0){
         greeting = greeting.substring(0, greeting.length-1 )
       }           
       println(greeting)
            
       //匿名函数
       var a:String = ""
       args.foreach{arg => {
           a += (arg + "_")
         }
       }       
       println(a)
       
       //去除花括号的匿名函数
       var b:String = ""
       args.foreach{arg => 
         b += (arg + "_._")
       }       
       println(b)
 
       //去除花括号的匿名函数
       var c:String = ""
       args.foreach(arg => 
         c += (arg + "_.__._")
       )
       println(c)
       
       foreachArr(args)
    }
  
    def foreachArr(args:Array[String]){
        args.foreach{
            arg => {
                println(arg)
            }
        }       
    }
}