# CMake generated Testfile for 
# Source directory: /home/chris/Code/C++/daily_report_system/v1.0
# Build directory: /home/chris/Code/C++/daily_report_system/v1.0/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(that-test-I-made "/home/chris/Code/C++/daily_report_system/v1.0/build/runUnitTests")
add_test(that-other-test-I-made "runUnitTests")
subdirs(lib/gtest-1.6.0)
