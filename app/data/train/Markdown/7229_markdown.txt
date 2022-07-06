+++
title = "LPTHW - Exercise 10: What was that?"
description = "Complete finished example of the tenth exercise from the Learn Python The Hard Way course. Exercise 10: What was that?"
categories = [
  "LPTHW",
]
date = "2016-05-09T22:59:00+01:00"
draft = "false"
url = "/code/lpthw-exercise-10-what-was-that/"
pageimage = ""
tags = [
  "Learn Python The Hard Way Exercise 10","LPTHW Exercise 10","Learn Python Exercise 10","LPTHW Ex 10"
]
slug = ""

+++
[LPTHW - Exercise 10](http://learnpythonthehardway.org/book/ex10.html)

I'm not sure exactly where, probably when butchering a Regex command, but I'd already encountered escape characters prior to the last exercise. 

It's obviously a very useful thing to learn, but coming back to it now I can't imagine not knowing what they are, how to look them up, and ultimately how to use them. That's one of the best feelings during the learning process!

Here's the code for this exercise, it was useful to go over this stuff again but I'm not sure how readable your code would get if you nest multiple new line/tab characters on a single line. 

```python
# \t mean a tab is inserted
tabby_cat ="\tI'm tabbed in."
persian_cat = "I'm split\non a line."
backslash_cat = "I'm \\ a \\ cat."

fat_cat = """
I'll do a list:
\t* Cat food
\t* Fishies
\t* Catnip\n\t* Grass
"""

print(tabby_cat)
print(persian_cat)
print(backslash_cat)
print(fat_cat)
``` 

## Learn Python The Hard Way - Study Drills

### 1. Memorize all the escape sequences by putting them on flash cards.

See right at the bottom of this post for my answer on this...

### 2. Use ''' (triple-single-quote) instead. Can you see why you might use that instead of """?

I would use them because it saves hitting the shift key, which saves some time. I suppose you'd just use whatever the convention is. Maybe you can tell how anal or sane a project is by the rules around the use of quote characters!

### 3. Combine escape sequences and format strings to create a more complex format.

Not sure what constitutes a 'more complex format'? Something that's eternally frustrating about tutorials are the lack of definitions of terminology. Tutors seem to forget that not everyone shares their personal definition of keywords. 

Thankfully the 'Common Student Questions' bit adds a little more clarity:

> "Take what you know about format strings and write some new code that uses format strings and the escape sequences from this exercise."

So not 'more complex', just 'new':
 
```python
# New code with format strings and escape characters

escape = '\nAh! \nhow \nmany \nnew \nlines \nin \nthis \nlist: {}'

print(escape.format('\n\tLoads!\n\tAnd some more'))
``` 

Output:

```text 
Ah! 
how 
many 
new 
lines 
in 
this 
list: 
        Loads!
        And some more
``` 

### 4. Remember the `{!r}` format? Combine `{!r}` with double-quote and single-quote escapes and print them out. Compare `{!r}` with `{!s}`. Notice how `{!r}` prints it the way you'd write it in your file, but `{!s}` prints it the way you'd like to see it?

I notice!
 
```python
# Study drill code with double/single quotes and %s or %r
print('Didn\'t you see {!r}, that\'s {!r} '.format("Michael\'s tops", "crazy"))
print('Didn\'t you see {!s}, that\'s {!s} '.format("Michael\'s tops", "crazy"))
``` 

Output:
 
```text
Didn't you see "Michael's tops", that's 'crazy' 
Didn't you see Michael's tops, that's crazy
```

## Escape sequences

Zed suggests that students memorise all of the following escape sequences (their format and what they do). I'm not sure that it's necessary to commit something like this to memory right now.

These characters are trivial to lookup (and I'm leaving this here for reference for myself) and I think I'll just move on and concentrate on learning the 'concepts' rather than rote memorisation of this bit.

|ESCAPE     |WHAT IT DOES|
|:---------:|------------|
|\\         |Backslash (\)
|\'         |Single-quote (')
|\"         |Double-quote (")
|\a         |ASCII bell (BEL)
|\b         |ASCII backspace (BS)
|\f         |ASCII formfeed (FF)
|\n         |ASCII linefeed (LF)
|\N{name}   |Character named name in the Unicode database (Unicode only)
|\r         |Carriage Return (CR)
|\t         |Horizontal Tab (TAB)
|\uxxxx     |Character with 16-bit hex value xxxx (Unicode only)
|\Uxxxxxxxx |Character with 32-bit hex value xxxxxxxx (Unicode only)
|\v         |ASCII vertical tab (VT)
|\ooo       |Character with octal value ooo
|\xhh       |Character with hex value hh

## Source files

As ever, [source files on GitHub](https://github.com/PuffinBlue/LPTHW).
