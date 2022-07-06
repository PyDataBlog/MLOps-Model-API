
var leaderboard2 = function(data) {
  return data.data.sort(function(a,b){return b.points-a.points;});
};

function liCreate(name,points) {
  var li = $('<li>'+name+'</li>');
  li.append('<span>'+points+'</span>');
}

$(document).ready(function() {
  // var sorted = leaderboard2(data);
  // for (var i=0; i<sorted.length; i++) {
  //   $('body').append('<div><p>'+sorted[i].name+' '+sorted[i].points+'</p></div>')
  // }
  // //problem here is with repeated DOM manipulation

  var studentArray = [];
  $.getJSON('http://192.168.1.35:8000/data.json').success(function(data){
    //using '$.getJSON' as opposed to $.ajax specifies
    //also, include both success and error handler to account for asynchrony.
    //i.e., if/when SUCCESS, execute some code block; if ERROR, execute another.
    console.log(data);
    var sorted = leaderboard2(data);
    for (var i=0; i<sorted.length; i++) {
      var student = ('<div><p>'+sorted[i].name+' '+sorted[i].points+'</p></div>');
      studentArray.push(student);
    }
    //^^ FIXED!!! Append entire array ^^
    console.log(studentArray);
    $('body').append(studentArray);

  })
    .error(function(error){
      console.log(error);
    });
});
