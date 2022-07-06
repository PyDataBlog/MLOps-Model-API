---
title:  "Line-Following Robot"
---


Here's goes my first "projects" post. Last semester I took a class called "Intro To Electromechanical Systems" (shout out to my professor Greg Lewin), a fun class about combining software and hardware to make some cool things. As you've probably guessed, one of the projects we had was to create a line following robot. But what I didn't mention in the title was that this robot didn't only follow lines, but also delivered packages! The robot can be seen below:

<img src="/assets/linefollowing1.JPG" width="70%" />

### Basically an Amazon delivery drone

The general idea was for this robot to get an order from a website we created, go and retrieve the package from our "warehouse", deliver it to the "customers home", and finally update the SQL database with help from some PHP scripts and a little bit of magic. We created the map that the robot following with electrical tape as the lines, and set specific points on the map to be warehouse A, B, or C and other points to be customer's addresses 1, 2, 3, and 4. The robot's "brain" (AI/singularity rant incoming...) was made using the arduino UNO, as well as the arduino wireless and motor shields. This allowed the robot to take the order wirelessly without needing to keep it connected to the computer that was putting the order in through the website. 

To make sure that the robot was able to properly follow the lines, light sensors and encoders on the motors were used to keep track of distance and whether the robot was actually on the line or not. Some psuedo code can be seen below:
 
If (not on line) {

Get back on the line

}

In all seriousness, the robot had two light sensors, and whenever one was giving out a high output (meaning it is seeing black), we adjusted its direction back towards the line.

Everything was wired together using a small breadboard to keep it neat. The body was designed in AutoCAD and laser cut before we put anything else together. Overall, it was pretty satisfying to have the system work in unison (after a ton of tests...I mean seriously a ton...). You can see the side and a closer view of the front of the robot below:

<img src="/assets/linefollowing2.JPG" width="70%" />

<img src="/assets/linefollowing3.JPG" width="70%" />

--- Jamel Charouel

Monday, August 3rd, 2015