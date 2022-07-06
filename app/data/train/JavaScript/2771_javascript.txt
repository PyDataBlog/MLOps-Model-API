var encodeDecode = function() {
   var randomNum = function (min, max) { // helper function for random numbers
     return Math.random() * (max - min) + min;
   };
   var insertBreak = function (counter) { //helper function to break lines @ 100 char
     if (counter % 100 === 0) {
       $('body').append("<br>");
     }
   };

   var encode = function (s) { //encodes a string
     var spl = s.split("");
     var lineCounter = 0;
     for (var i=0; i < spl.length; i++) {
       $('body').append("<span class='encoded' hidden>" + spl[i] + "</span>");
       var r = randomNum(20,30);
       for (var j=0; j < r; j++) {
         insertBreak(lineCounter);
         lineCounter++;
         var q = randomNum(33,126);
         $('body').append("<span>" + String.fromCharCode(q) + "</span>");
       }
     }
   };

   var decode = function () { //decodes the page
     var decodeString = "";
     $('[hidden]').each(function() {
       decodeString += $(this).text();
       $(this).remove();
     });
     $('span, br').remove();
     $('body').append("<span class='decoded'>" + decodeString + "</span>");
   };

   if ($('body').children('span.encoded').length > 0) {
      decode();
   } else if ($('body').children('span.decoded').length > 0) {
      var s = $('span.decoded').text();
      $('span.decoded').remove();
      encode(s);
   } else {
     encode("The challenge was found by running: $('body').children().toggle(); Note that even the line breaks from the challenege were included in my script.  We should grab lunch, don't you think? "); //starter string to encode / decode
   }
};
$( document ).ready(function() {
	encodeDecode();
});

