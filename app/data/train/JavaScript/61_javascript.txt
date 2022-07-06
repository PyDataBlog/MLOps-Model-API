// Release 1: User Stories
// As a user, I want to be able to create a new grocery list. After that, I need to be able to add an item with a quantity to the list, remove an item, and update the quantities if they change. I need a way to print out the list in a format that is very readable.

// Release 2: Pseudocode
// input: string of items separated by spaces
// output: object
// create a new object as new variable
// convert string to array (split)
// take each item in array and add to object as a property with a default quantity/value of 1
//

// Release 3: Initial Solution
// function to create list
// var foodList = ("salmon iceCream macAndCheese")
// var groceryList = {};

// var createList = function(foodList) {
//   var foodArray = foodList.split(" ");
//   for (var i = 0; i < foodArray.length; i++){
//     groceryList[(foodArray[i])] = 1;
//   }
//   console.log(groceryList);
// }

// createList(foodList)

// // function to add item to list

// var addItem = function(newItem) {
//   groceryList[newItem] = 1;
//   console.log(groceryList);
// }

// addItem("peas")


// // function to remove item from list

// var removeItem = function(itemToLose) {
//   delete groceryList[itemToLose];
//   console.log(groceryList);
// }

// removeItem("peas")

// // function to update quantity

// var updateList = function(updateItem, newQuantity) {
//   groceryList[updateItem] = newQuantity;
//   console.log(groceryList);
// }

// updateList("macAndCheese", 5)

// // function to display list

// var displayList = function(groceryList) {
//   for (food in groceryList) {
//     console.log(food + ": " + groceryList[food]);
//   }
// }

// displayList(groceryList)

// Release 4: Refactor
// function to create list

var groceryList = {};
var displayList = function(groceryList) {
  console.log("Your Grocery List:")
  for (food in groceryList) {
    console.log(food + ": " + groceryList[food]);
  }
  console.log("----------")
}

var createList = function(foodList) {
  var foodArray = foodList.split(" ");
  for (var i = 0; i < foodArray.length; i++){
    groceryList[(foodArray[i])] = 1;
  }
  displayList(groceryList);
}

var addItem = function(newItem) {
  groceryList[newItem] = 1;
  displayList(groceryList);
}

var removeItem = function(itemToLose) {
  delete groceryList[itemToLose];
  displayList(groceryList);
}

var updateList = function(updateItem, newQuantity) {
  groceryList[updateItem] = newQuantity;
  displayList(groceryList);
}

var foodList = ("funfettiMix bananas chocolateCoveredAlmonds")
createList(foodList)
addItem("peaches")
updateList("peaches", 20)
removeItem("bananas")

// Release 5: Reflect
// What concepts did you solidify in working on this challenge? (reviewing the passing of information, objects, constructors, etc.)
// I solidified accessing different properties in an object. I was able to add strings from an array into an empty object and set their default value. To change those values I knew I needed to access the property using bracket notation, and change the value it was = to.
// What was the most difficult part of this challenge?
// I forgot I needed to convert the string to an array, but once I did that with the .split(" ") method, all of the strings were easily accessible to add to the new object.
// Did an array or object make more sense to use and why?
// This was weirdly WAY easier with JavaScript than it was initially with Ruby (probably because we were on our second week of Ruby at this point!). It was so easy to add each string from an array into the object as a property and set it's default. Accessing these properties to update or delete was made easier by using bracket notation. Instead of complicated hash methods and having to convert strings to arrays to hashes, all I had to do was split the string and add each string to the object with a default value.