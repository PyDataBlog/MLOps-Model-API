// node/this-3.js

var object = {
  id: "xyz",
  printId: function() {
    console.log('The id is '+
        this.id + ' ' +
        this.toString());
  }
};

// setTimeout(object.printId, 100);
var callback = object.printId;
callback();
