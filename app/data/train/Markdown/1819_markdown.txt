---
layout: post
title:  "MySQL Reflection"
date:   2016-11-16
categories: "projects"
assignment: "true"
---
[Link to Github Repository](https://github.com/ldinkins/malily5)

We continued working on the bash script from Assignment 4 by adding a connection to MySQL. First, we created the database malily if it didn't already exist. Then we fed a dump file into the database. Afterwards, we added the current responses to the database and then outputted the database into the dump file. This created an import-export loop that anyone could use this script and the dump file to have access to the information in the table as well as add to the table.

Originally, we didn't make the script like this. We only had it create a database and add information to a table, making it local. To make sure that Lily and I could both access each other's responses, we created the import dump and the export dump. We ran into some troubles with this, mainly syntax problems. To debug, the script had a line before the insert statement that printed the database and a line after that prints the database. We put this line in to make sure that our additions were going in from different systems. Now, there's only one final line that prints the database table. Our syntax problems were from importing the dump, and it took us a while to realize that mysqldump is a separate command and cannot go into the block of mysql commands. One final thing we had to do was sync our databases to make sure that what was in Lily's database was in mine.

Overall, I thought this assignment was a good refresher in working with MySQL. I previously worked with it in Perl, and I had the most trouble with remembering syntax. Stack Overflow really helped with that though. Also, it was great to work with Lily on this. We tried to work on each step together instead of dividing and conquering. On a personal level, I loved explaining things that made more sense to me than they did to her because I think that being able to break down technical concepts verbally reinforces what I already know. It also made clear the points that I didn't know as well as others, and we Googled our way through it. It was a great learning experience. I think it's easy to bulldoze and just do something yourself if you have a better idea of what's going on, but working as a team was much more rewarding and made the assignment fun.