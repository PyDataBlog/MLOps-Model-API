export isTrue, isFalse

isTrue = (x::Bool) -> {
    if !x
      throw(AssertionError(string("Expected <",x , "> to be true.")))
    end
  }

isFalse = (x::Bool) -> { 
    if x
      throw(AssertionError(string("Expected <",x , "> to be false.")))
    end
  }