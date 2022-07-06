// Welcome. In this kata, you are asked to square every digit of a number.
//
// For example, if we run 9119 through the function, 811181 will come out.
//
// Note: The function accepts an integer and returns an integer

function squareDigits(num){
  let digits = (""+num).split("");
  let arr=[];
  for (var i = 0; i < digits.length; i++) {
    arr.push(Math.pow(digits[i],2));
  }
  return +arr.join('')
}

//top codewars solution
function squareDigits(num){
  return Number(('' + num).split('').map(function (val) { return val * val;}).join(''));
}
