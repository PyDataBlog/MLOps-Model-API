# Copyright (c) 2015, Ruslan Baratov, David Hirvonen
# All rights reserved.

if(DEFINED POLLY_FLAGS_FNO_ALIGNED_ALLOCATION_CMAKE_)
  return()
else()
  set(POLLY_FLAGS_FNO_ALIGNED_ALLOCATION_CMAKE_ 1)
endif()

include(polly_add_cache_flag)

string(COMPARE EQUAL "${ANDROID_NDK_VERSION}" "" _not_android)

# TODO: test other platfroms, CMAKE_CXX_FLAGS_INIT should work for all
if(_not_android)
  polly_add_cache_flag(CMAKE_CXX_FLAGS "-fno-aligned-allocation")
  polly_add_cache_flag(CMAKE_C_FLAGS "-fno-aligned-allocation")
  polly_add_cache_flag(CMAKE_Fortran_FLAGS "-fno-aligned-allocation")
else()
  polly_add_cache_flag(CMAKE_C_FLAGS_INIT "-fno-aligned-allocation")
  polly_add_cache_flag(CMAKE_CXX_FLAGS_INIT "-fno-aligned-allocation")
  polly_add_cache_flag(CMAKE_Fortran_FLAGS_INIT "-fno-aligned-allocation")
endif()

