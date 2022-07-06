# Copyright (c) 2016, Ruslan Baratov, Luc Michalski
# All rights reserved.

# !!! DO NOT PLACE HEADER GUARDS HERE !!!

include(hunter_add_version)
include(hunter_cacheable)
include(hunter_cmake_args)
include(hunter_download)
include(hunter_pick_scheme)

hunter_add_package(OpenCV) # OpenCV
hunter_add_package(Boost) # Boost
hunter_add_package(Eigen) # Boost
hunter_add_package(cereal) # cereal

hunter_add_version(
    PACKAGE_NAME
    4DFace
    VERSION
    "0.7.0-p1"
    URLa
    "https://github.com/ConfusedReality/pkg_face-tracking_4dface/archive/v0.1.0-p0.tar.gz"
    SHA1
    b34e7a087513b579368927642e576722cbe067e8
    )

hunter_add_version(
    PACKAGE_NAME
    4DFace
    VERSION
    develop
    URL
    "file:///Volumes/HardDrive/go/src/github.com/lucmichalski/wormz/shared/git/packages/pkg_face-tracking_4dface/archive/develop.tar.gz"
    SHA1
    f8cbbdcbaf758da584de2d2388fd8818c236c682
)

hunter_cmake_args(
    4DFace
    CMAKE_ARGS 
	BUILD_EXAMPLES=OFF
    BUILD_DOCUMENTATION=OFF
	BUILD_UTILS=OFF
    BUILD_GLSAMPLES=OFF
    INSTALL_DOC=OFF
    WARNINGS_ARE_ERRORS=OFF
    WHOLE_PROGRAM_OPTIMIZATION=OFF
)

hunter_pick_scheme(DEFAULT url_sha1_cmake)
hunter_cacheable(4DFace)
#hunter_download(PACKAGE_NAME 4DFace)

hunter_download(
    PACKAGE_NAME 4DFace
    # Explicit dependencies since LLVM is not hunterized
    PACKAGE_DEPENDS_ON OpenCV Boost Eigen cereal
)
