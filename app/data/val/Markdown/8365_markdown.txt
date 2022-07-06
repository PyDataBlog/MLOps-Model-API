---
title: filterNot -
---
//[Kotlin utilities](../../index.html)/[it.czerwinski.kotlin.util](../index.html)/[Failure](index.html)/[filterNot](filter-not.html)



# filterNot  
[common]  
Brief description  


Returns the same [Success](../-success/index.html) if the predicate is not satisfied for the value. Otherwise returns a [Failure](index.html).



#### Return  


The same [Success](../-success/index.html) if the predicate is not satisfied for the value. Otherwise returns a [Failure](index.html).



## Parameters  
  
common  
  
|  Name|  Summary| 
|---|---|
| predicate| <br><br>Predicate function.<br><br>
  
  
Content  
open override fun [filterNot](filter-not.html)(predicate: ([Nothing](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-nothing/index.html)) -> [Boolean](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-boolean/index.html)): [Try](../-try/index.html)<[Nothing](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-nothing/index.html)>  



