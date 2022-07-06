# Lesson-1---Review

### Create a song class
#### Attributes:
1. Title
2. Band members: a dictionary with name as the key and instrument as the values.
3. (Highest) place on the charts.

#### Methods:
1. __init__
   Has a filepointer as an optional parameter. If filepointer is None have the user input the attributes. If the filepointer is not None input the song from the file. 
2. __str__
   Output the attributes in a nice way
3. save
   Save a song to a file, the filepointer should be a parameter.
  
### Create an album class
#### Attributes:
1. Title
2. Year
3. Producer
4. Songs - a list of songs each of which are of type class song.

#### Methods:
1. __init__
   Has a filepointer as an optional parameter. If filepointer is None have the user input the attributes. If the filepointer is not None input the album from the file. In both cases, when you input a song make sure to use the songs method for input.
   
   The song attribute should be an optional parameter. If the value is None keep inputting songs until the user is done.
2. __str__
   Output the attributes of the album in a nice way.
3. save
   saves the entire album to a file, makes use of the song.save attribute for the songs. The filepointer is a parameter.

### Create a menu
#### Options:
1. Input a new album
2. Print albums
3. Quit

When your program is opened it should read in the albums into a list of albums from the file, and when the user quits it should write the albums in the list to a file. Option 1 should add a new album to the album list.
