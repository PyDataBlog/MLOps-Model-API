// 19. Write a JavaScript function that returns array elements larger than a number. 
//two agrs - an array and a number to be larger than
function isGreater(arr, num) {
  //set up an array to contain the results
  var resultArray = [];
  //iterate through based on length of the arr
  for(var i = 0; i < arr.length; i++) {
    //if current arr value is greater than num
    if(arr[i] > num) {
      //push result to resultArray
      resultArray.push(arr[i]);
    }
  }
  //log results
  console.log(resultArray);
}
