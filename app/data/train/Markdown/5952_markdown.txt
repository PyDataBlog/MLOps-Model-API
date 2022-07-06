4b1w
====

Automatic 4 Bilder 1 Wort solving with a dictionary

You will need a dictionary like this: https://github.com/mrpg/dict-de

This is probably the best one you can find, and it is already in the appropriate format, so take it and use it.

After you have successfully compiled your binary...

> g++ a.cpp -std=c++0x -O3 -Wall -o4b1w

**or**

> clang++ a.cpp -std=c++0x -O3 -Wall -o4b1w

... you can invoke the program like this:

> cat dict-de/dict_de.txt | ./4b1w TRKOEKLNYBAG 5 > results.txt

Here, dict-de/dict_de.txt is the dictionary, TRKOEKLNYBAG are the possible
characters and 5 is the resulting word length.

Behold! If the possible characters include umlauts (äöüß, etc.) and you are
running 4b1w on a UTF-8 platform while your dictionary is using ISO-8859-1
(like the standard dictionary) you have to invoke the program like this:

> cat dict-de/dict_de.txt | ./4b1w $(echo ÜFSNTÄFLUTEMMR | iconv -t ISO-8859-1) 10 > results.txt

You can then find your results in the file results.txt.

That's all!
