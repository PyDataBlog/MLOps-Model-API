# Copyright (c) 2016, Luc Michalski
# All rights reserved.

# !!! DO NOT PLACE HEADER GUARDS HERE !!!

include(hunter_add_version)
#include(hunter_cacheable)
include(hunter_download)
include(hunter_pick_scheme)
include(hunter_cmake_args)

hunter_add_version(
    PACKAGE_NAME
    OpenMP
    VERSION
    "0.0.1-p0"
    URL
    "https://github.com/ConfusedReality/openmp/archive/0.0.1-p0.tar.gz"
    SHA1
    "d4b2833b5f090c292f8ad5297503bc9309009516"
)

hunter_add_version(
    PACKAGE_NAME
    OpenMP
    VERSION
    develop
    URL
    "file:///Volumes/HardDrive/go/src/github.com/lucmichalski/wormz/shared/git/packages/openmp/archive/develop.tar.gz"
    SHA1
    7f5c2b99cd8c2b149aa541a7580a31267c1d245f
) 

hunter_cmake_args(
    OpenMP
    CMAKE_ARGS
    LIBOMP_STANDALONE_BUILD=ON
    LIBOMP_ENABLE_SHARED=OFF
    LIBOMP_USE_DEBUGGER=OFF
    LIBOMP_USE_STDCPPLIB=OFF
    LIBOMP_USE_ADAPTIVE_LOCKS=OFF
    LIBOMP_STATS=OFF
    LIBOMP_OMPT_DEBUG=OFF
    LIBOMP_OMPT_SUPPORT=OFF
    LIBOMP_OMPT_BLAME=OFF
    LIBOMP_OMPT_TRACE=OFF
)

#hunter_cacheable(OpenMP)
hunter_pick_scheme(DEFAULT url_sha1_cmake)
hunter_download(PACKAGE_NAME OpenMP)

