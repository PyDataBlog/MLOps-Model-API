find_package(PkgConfig)
if(PKG_CONFIG_FOUND)
  pkg_check_modules (JSONCPP jsoncpp)
endif()

find_path(JSONCPP_INCLUDE_DIR
  NAMES json/features.h
  PATH_SUFFIXES jsoncpp
  PATHS ${JSONCPP_PKGCONF_INCLUDE_DIRS}
)

find_library(JSONCPP_LIBRARY
  NAMES jsoncpp
  PATHS ${JSONCPP_PKGCONF_LIBRARY_DIRS}
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(jsoncpp DEFAULT_MSG JSONCPP_LIBRARY JSONCPP_INCLUDE_DIR)
mark_as_advanced(JSONCPP_INCLUDE_DIR JSONCPP_LIBRARY)
