#!/usr/bin/python

import sys
import re

re_valid_email = re.compile(r'^[-_0-9a-zA-Z]+@[0-9a-zA-Z]+\.[0-9a-zA-Z]{1,3}$')

def valid_email(s):
    return not (re_valid_email.search(s) == None)

N = int(raw_input().strip())
A = []
for i in range(N):
    A += [ str(raw_input().strip()) ]
A.sort()

V = filter(valid_email, A)
print V

#### INPUT ##
## 3
## lara@hackerrank.com
## brian-23@hackerrank.com
## britts_54@hackerrank.com
##
#### OUTPUT ##
## ['brian-23@hackerrank.com', 'britts_54@hackerrank.com', 'lara@hackerrank.com']

#### INPUT ##
## 5
## dheeraj-234@gmail.com
## itsallcrap
## harsh_1234@rediff.in
## kunal_shin@iop.az
## matt23@@india.in
## 
#### OUTPUT ##
## ['dheeraj-234@gmail.com', 'harsh_1234@rediff.in', 'kunal_shin@iop.az']

