![GeneralAssemb.ly](../../img/icons/FEWD_Logo.png)

#FEWD - Review, Process, and Debugging

###Lesson 11 - Mar 14, 2016

"What was the hardest part of learning to code? 

It’s learning to not let errors and complicated functions get you down. Everyone makes mistakes and it’s the process of debugging them that teaches you how to be a better coder." - <a href="http://imagirlwhocodes.com/post/140868820390/amanda-xu-a-swerve-in-another-direction">Amanda Xu</a>

---


##Agenda

*	Surveys, please!
*	Exit Ticket Review
*	Debugging
*	Assignment 5 review: from pseudocode to implementation

---


##Exit Tickets

*	What is a good way to memorize the most used functions in Jquery/javascript? 

Answer: Once you encounter a jQuery function, you'll find that many of them are named pretty transparently after what they do (`.html` and `.css` for example).  But as you're starting out, you may still need to look up the documentation or StackOverflow to jog your memory -- that's ok!

*	How in the hell do I get good at this?

Answer: Persistence, practice, and patience with yourself!


---

## Resources for practicing


*	<a href="http://tympanus.net/codrops/">Codrops tutorials and playground</a>
*	<a href="https://www.smashingmagazine.com/2008/09/jquery-examples-and-best-practices/">Continue to learn about best practices</a>
*	<a href="https://www.codecademy.com/learn/jquery">Code Academy jQuery course</a>
* 	<a href="http://jqexercise.droppages.com/">jQuery drills</a> - these range in difficulty and don't offer much to help you learn, but it's great for practicing!


---

##Debugging

<img src="http://www.wired.com/images_blogs/wiredscience/2013/12/moth-660x548.jpg" style="height: 80%">

The very first "bug", <a href="http://www.wired.com/2013/12/googles-doodle-honors-grace-hopper-and-entomology/">discovered by Grace Hopper</a>.


---

##Debugging tips

*	The console is your friend:
	*	The first thing you should always do if your code starts doing things contrary to your expectations is open up your console.  A lot of times console will just tell you what went wrong!
	*	Use `console.log` whenever you need to check if something is working: write out variable values to check that they are updating correctly, log out messages to verify your event handlers are firing correctly, etc.  [You can even log out jQuery elements!]
*	When a part of your code doesn't work, take a step back and ask yourself these questions:
	*	What did I expect to happen?
	*	What happened instead?
	*	Where in the place in my code where this functionality is being implemented? 
*	Instead of attempting to throw more code at an already buggy codebase, simplify.


---

##Debug-a-long

*	Fixing errors with the <a href="starter/debugging-relaxr">Relaxr blog exercise</a>.
*	Fixing errors with the <a href="starter/debugging-compare-that">Compare That exercise</a>.


---

## Assignment 5 : pseudocode to implementation

Pseudocoding the CitiPix app:

*	Need to set up a click listener + handler for the submit button.
*	When the submit button is clicked, need to get the input value.
	*	Verify (using `console.log`) that you are getting the value before you continue to the next parts.
*	Once you have the input value, our program needs to **DECIDE** if we should add a class to the body based on what was typed.  *Because each of these should work approximately the same way, if we figure out one case, we can carry over the technique to the other cases.*
	*	If the user typed "New York" or "New York City" or "NYC", we should see the nyc.jpg background
	*	If the user typed "San Francisco" or "SF" or "Bay Area", we should see the sf background
	*	If the user typed "Los Angeles" or "LA" or "LAX", we should see the la.jpg background
	*	If the user typed "Austin" or "ATX", we should see the austin.jpg background
	*	If the user typed "Sydney" or "SYD", we should see the sydney.jpg background				
---

## More Practice: Number Guesser

We will build a number guesser game where the computer generates a random number from 1-10, and the user enters numbers into the input field until the number they enter is a match.  When the user's guess is incorrect, the program will tell user if they need to guess higher or lower.

<a href="http://codepen.io/emmacunningham/pen/pyNJJy?editors=1010">Starter code</a>

*	Write pseudocode for the logic flow of your program
*	Convert the pseudocode to real JavaScript code, working on things piece-by-piece


---

##Homework 

*	Work on the HTML + CSS for your Final Project
*	Surveys if you haven't yet filled yours out

---

##Exit Tickets - Lesson #11, Topic: Review & Debugging

###Please fill out the <a href="https://docs.google.com/forms/d/1Iw2zghHfGgeM1p1G16F6kLi7KViv28tG3HVNnoM3PAc/viewform">exit ticket</a> before you leave


