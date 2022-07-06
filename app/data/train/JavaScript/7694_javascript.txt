  var arrayOfInts = [1, 10, -5, 1, -100];
  
  function maxProduct(arr) {
    var smallest = 0;
    var almostSmallest = 0;
    var largest = 0;
    var second = 0;
    var third = 0;
    for(var i = 0; i < arr.length; i++ ) {
      if(arr[i] > largest){
        third = second
        second = largest 
        largest = arr[i]
      } else if(arr[i] > second) {
        third = second
        second = arr[i]
      } else if(arr[i] > third){
        third = arr[i]
      } else if( arr[i] < 0) {
        if(arr[i] < smallest) {
          almostSmallest = smallest
          smallest = arr[i];
        } else {
          if( arr[i] < almostSmallest) {
            almostSmallest = arr[i]
          }
        }
      }
    }
    if(smallest * almostSmallest > second * third) {
      return largest * almostSmallest * smallest
    }
    return largest * second * third
  }
  
  maxProduct(arrayOfInts)