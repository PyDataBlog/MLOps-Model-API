# CMake generated Testfile for 
# Source directory: /home/elyas/RayTracerDE
# Build directory: /home/elyas/RayTracerDE/source
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(tests "/home/elyas/RayTracerDE/source/build/Release/tests")
subdirs(external/glfw-3.0.3)
subdirs(framework)
subdirs(source)
subdirs(tests)
