# Regular Expressions

## Overview

This week we are focusing on constructing regular expressions to match patterns 
in strings. Regular expressions are incredibly powerful tools that make what would 
otherwise be tedious and tiresome search and replace operations a breeze to do! 

If you did not bring a laptop today, please partner up with someone else. In
your README.md, make sure **both you and your partner's names** are listed.

Here are some useful links:

- [Regular Expressions][regex] in general
- [Regular Expressions in Scala][regexscala]

## Programming Task

Today you will be parsing the entirety of Lewis Carroll's Alice in Wonderland. 
You will write a number of regular expressions to find certain patterns 
throughout the text. You need only edit the Main.scala file in the location 
indicated.

**You should be committing between steps in order to save your progress.**

1. In your repository, create a folder called **dis08**.

1. Create a **README.md** file that contains the lines:

    - CMPSCI 220 Programming Methodology
    - Discussion 8 Activity
    - Your name
    - Your Spire ID
    - (Add two more lines with your partner's information if you have one)

1. Download alice.txt and Main.scala from this git repository. Your dis08 
	 directory should look as follows:

    ```
    - README.md
    - alice.txt
    - Main.scala
    ```

1. Write the following regular expressions in Main.scala. For each of these 
	 expressions, simply print out the number of matches, not everything that was 
	 found.

    2. Find all occurrences of 'Alice'.
    2. Find all occurrences of 'off with his/her head'. 'Off' may or may not be 
    	 capitalized and should account for 'his' or 'her'.
    2. Find all occurrences where 'the' is between two other words.
    2. Find all occurrences where the last word in an exclamatory sentence is 
    	 capitalized.
    2. Find all distinct occurrences where a word contains, but does does not 
    	 begin with, the word 'cat'.
    2. Find all distinct occurrences where a word begins with 'c' and ends with 
    	 'e'.

1. Expected output:
	```scala
	398
	6
	73
	1340
	11
	35
	```

## Deliverables

Once you have written all of the above tests, you should submit the whole dis08
folder, including the skeleton code, to your Gitblit repository as usual. (See
the next section.)

**NOTE:** A portion of this assignment's grade is based on whether or not it
compiles. We suggest that you make sure your code compiles *before you submit
it*.

## Submitting

You will not be submitting this assignment to Moodle. Instead, we will be using
*tagging*, just like the previous discussion sections.

**You will be submitting this assignment at the end of the discussion section.
Do not worry if you do not have everything finished.**

1. Make sure everything is added, committed, and pushed.

2. Create a tag: `git tag -a dis08 -m "dis08"`

3. Push the tag to the gitblit repository: `git push origin dis08`

We will use this tag when grading. Make sure that you create the tag right before your final push.

[regex]: http://www.regular-expressions.info/quickstart.html
[regexscala]: http://www.scala-lang.org/api/current/index.html#scala.util.matching.Regex
