#! /bin/bash

# sort-numbers.sh -- This script asks the user to input any 10 numbers of choice and each number is added to a file called unsorted.txt and finally the program prints out the sorted list of numbers.

# Author : Prince Oppong Boamah<regioths@gmail.com>
# Date : 7 July 2015

echo  "Hello and please input any 10 numbers of your choice "
read NUM
echo $NUM > unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
read NUM
echo $NUM >> unsorted.txt
echo " These are the numbers you entered and they have been sorted out. " 
    cat unsorted.txt | sort |uniq



