if(DEFINED HUNTER_CMAKE_PROJECTS_LIBGIT2_HUNTER_CMAKE_)
  return()
else()
  set(HUNTER_CMAKE_PROJECTS_LIBGIT2_HUNTER_CMAKE_ 1)
endif()

include(hunter_add_version)
include(hunter_download)
include(hunter_pick_scheme)

hunter_add_version(
    PACKAGE_NAME Libgit2 
    VERSION "0.23.1"
    URL "https://github.com/libgit2/libgit2/archive/v0.23.1.tar.gz" 
    SHA1 ebe7c592506dc82a2c0c1011723e8477f31b4aa8 
)

hunter_cmake_args(Libgit2 CMAKE_ARGS BUILD_SHARED_LIBS=ON)

hunter_pick_scheme(DEFAULT url_sha1_cmake)
hunter_download(PACKAGE_NAME Libgit2)
