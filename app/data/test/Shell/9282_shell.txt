#!/bin/bash

# The MIT License (MIT)
# 
# Copyright (c) 2015 Southern Nazarene University Computer Science/Network Engineering Department
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# ******************************************************************************
# Ch. 05 - The Shell

# ******************************************************************************
# Redirecting Standard Input and Standard Output

# Redirect output of ls to a file, overwriting any previous contents
ls /usr/bin/ > binfiles.txt
less binfiles.txt

# Redirect output of ls, but append it if the file already exists
ls /sbin/ >> binfiles.txt

# Redirect standard input
cat < binfiles.txt

# Throw the output of the ls command away
ls /etc/ > /dev/null

# Dump the contents of /dev/null, which is empty
cat < /dev/null


# ******************************************************************************
# Pipelines

# A command using pipes
grep -i 'ed' names.txt | sort | uniq

# Commands not using pipes
grep -i 'ed' names.txt > ed-names.txt
sort < ed-names.txt > sorted-ed-names.txt
uniq < sorted-ed-names.txt
rm ed-names.txt sorted-ed-names.txt

# A command to find how many times I am logged into the system using pipes
who | grep ${USER} | wc -l
# The same command without pipes
who > who-is-logged-in.txt
grep $USER who-is-logged-in.txt > my-logins.txt
wc -l my-logins.txt
rm who-is-logged-in.txt my-logins.txt


# ******************************************************************************
# Running Commands in the Background

# Start a GUI command in the background
pluma &

# Start a GUI command in the foreground
pluma
# Use CONTROL-Z to suspend the process
# List all the processes in this shell
jobs
# Bring it back to the foreground
fg 1

# Suspend the process again using CONTROL-Z
# Send it to the background
bg


# ******************************************************************************
# Wildcards

# List all the files in the directory
ls
# List only the ones matching the pattern using '?'
ls memo?
ls to?
# List only the ones matching the pattern using '*' at the end
ls memo*
# List only the ones matching the pattern using muliple '*'
ls *emo*
# List only the ones matching the pattern using [ ]
ls memo[124]
ls memo[1-3]
ls [to]*


# ******************************************************************************
# Builtin Commands

# List all the builtin commands
help

