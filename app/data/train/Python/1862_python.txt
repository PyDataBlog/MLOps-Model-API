#!/usr/bin/python3.3

import unittest
import sys
sys.path.append("/home/hazel/Documents/new_linux_paradise/paradise_office_site/sandbox_v1.0/cygnet_maker/cy_data_validation")
from datetime import time
from time_conv import Time

class TimeTestCase(unittest.TestCase):
  ''' Tests with numbered degrees of bad or good data, on a scale of 0=baddest to 10=goodest '''  

  def setUp(self):
    self.time = Time.check_time("")

  # A string
  def test_vbad0(self):
    self.time = Time.check_time("iutoeht")
    correct_time = time(00, 00, 00)
    self.assertEqual( correct_time, self.time)

  # An out of range whole number
  def test_bad1(self):
    self.time = Time.check_time("52304")
    correct_time = time(00, 00, 00)
    self.assertEqual( correct_time, self.time)

  # Two out of range whole numbers
  def test_bad2(self):
    self.time = Time.check_time("70 80")
    correct_time = time(23, 59, 00)
    self.assertEqual( correct_time, self.time)

  # hours, minutes and seconds formatted crap
  def test_middle3(self):
    self.time = Time.check_time("03 - 32/74")
    correct_time = time(3, 32, 59)
    self.assertEqual( correct_time, self.time)

  # : in between hours and minutes
  def test_good4(self):
    self.time = Time.check_time("03:32:50")
    correct_time = time(3, 32, 50)
    self.assertEqual( correct_time, self.time)

  # removing am or pm
  def test_vgood5(self):
    self.time = Time.check_time("3:35pm")
    correct_time = time(15, 35, 00)
    self.assertEqual( correct_time, self.time)


  def tearDown(self):
    self.time = ""
    correct_time = ""

if __name__ == '__main__':
  unittest.main()
