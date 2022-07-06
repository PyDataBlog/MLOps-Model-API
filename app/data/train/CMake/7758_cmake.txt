#
#   Copyright (C) 2014-15 Chris Copsey
#
#   This file is part of ccFOAM.
#
#   ccFOAM is free software: you can redistribute it and/or modify it under the
#   terms of the GNU General Public License as published by the Free Software
#   Foundation, either version 3 of the License, or (at your option) any later
#   version.
#
#   ccFOAM is distributed in the hope that it will be useful, but WITHOUT ANY
#   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
#   details.
#
#   You should have received a copy of the GNU General Public License along
#   with ccFOAM.  If not, see <http://www.gnu.org/licenses/>.
#

include(FindPackageHandleStandardArgs)

if (NOT BOOST_SYSTEM_LIBRARY OR NOT BOOST_INCLUDE_DIR)

  find_path(BOOST_INCLUDE_DIR
    NAMES "boost/thread.hpp"
    HINTS ${BOOST_INCLUDE_DIR} ${BOOST_ROOT}/include ${CMAKE_EXTRA_INCLUDES}
  )

  if (NOT BOOST_INCLUDE_DIR)
    message(SEND_ERROR "Can not locate BOOST include directory")
  endif()

  find_library(BOOST_SYSTEM_LIBRARY
    NAMES "boost_system"
    HINTS ${BOOST_LIBRARY_DIR} ${BOOST_ROOT}/lib ${CMAKE_EXTRA_LIBRARIES}
  )

  if(NOT BOOST_SYSTEM_LIBRARY)
    message(SEND_ERROR "Can not locate BOOST_SYSTEM library")
  endif()    

endif()

find_package_handle_standard_args(BOOST_SYSTEM
  DEFAULT_MSG
  BOOST_SYSTEM_LIBRARY
  BOOST_INCLUDE_DIR
)
