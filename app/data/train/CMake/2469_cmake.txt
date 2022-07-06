# Install script for directory: /home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel

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

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "gruel_devel")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/gruel" TYPE FILE FILES
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/api.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/attributes.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/high_res_timer.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/msg_accepter.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/msg_accepter_msgq.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/msg_queue.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/msg_passing.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/pmt.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/pmt_pool.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/pmt_sugar.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/realtime.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/sys_pri.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/thread_body_wrapper.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/thread_group.h"
    "/home/aviral/GNU-Niyantran/gr36/gruel/src/include/gruel/thread.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "gruel_devel")

