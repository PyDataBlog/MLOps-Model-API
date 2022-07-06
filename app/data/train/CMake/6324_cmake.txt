# Install script for directory: /home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/usr/local")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "Release")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "1")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/python2.7/dist-packages/gnuradio/modtool" TYPE FILE FILES
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/__init__.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/cmakefile_editor.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/code_generator.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/grc_xml_generator.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_add.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_base.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_disable.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_help.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_info.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_makexml.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_newmod.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/modtool_rm.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/parser_cc_block.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/templates.py"
    "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/util_functions.py"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/python2.7/dist-packages/gnuradio/modtool" TYPE FILE FILES
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/__init__.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/cmakefile_editor.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/code_generator.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/grc_xml_generator.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_add.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_base.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_disable.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_help.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_info.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_makexml.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_newmod.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_rm.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/parser_cc_block.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/templates.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/util_functions.pyc"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/__init__.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/cmakefile_editor.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/code_generator.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/grc_xml_generator.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_add.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_base.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_disable.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_help.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_info.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_makexml.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_newmod.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool_rm.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/parser_cc_block.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/templates.pyo"
    "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/util_functions.pyo"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/gnuradio/modtool" TYPE DIRECTORY FILES "/home/aviral/GNU-Niyantran/gr36/gr-utils/src/python/modtool/gr-newmod")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")
  list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
   "/usr/local/etc/gnuradio/conf.d/modtool.conf")
FILE(INSTALL DESTINATION "/usr/local/etc/gnuradio/conf.d" TYPE FILE FILES "/home/aviral/GNU-Niyantran/build/gr36/gr-utils/src/python/modtool/modtool.conf")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utils")

