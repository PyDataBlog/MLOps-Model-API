# Install script for directory: /home/huanghao/mysql-connector-c/include

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "./install")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "RelWithDebInfo")
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

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Development")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/huanghao/mysql-connector-c/include/mysql.h"
    "/home/huanghao/mysql-connector-c/include/mysql_com.h"
    "/home/huanghao/mysql-connector-c/include/mysql_time.h"
    "/home/huanghao/mysql-connector-c/include/my_list.h"
    "/home/huanghao/mysql-connector-c/include/my_alloc.h"
    "/home/huanghao/mysql-connector-c/include/typelib.h"
    "/home/huanghao/mysql-connector-c/include/my_dbug.h"
    "/home/huanghao/mysql-connector-c/include/m_string.h"
    "/home/huanghao/mysql-connector-c/include/my_sys.h"
    "/home/huanghao/mysql-connector-c/include/my_xml.h"
    "/home/huanghao/mysql-connector-c/include/mysql_embed.h"
    "/home/huanghao/mysql-connector-c/include/my_pthread.h"
    "/home/huanghao/mysql-connector-c/include/decimal.h"
    "/home/huanghao/mysql-connector-c/include/errmsg.h"
    "/home/huanghao/mysql-connector-c/include/my_global.h"
    "/home/huanghao/mysql-connector-c/include/my_getopt.h"
    "/home/huanghao/mysql-connector-c/include/sslopt-longopts.h"
    "/home/huanghao/mysql-connector-c/include/my_dir.h"
    "/home/huanghao/mysql-connector-c/include/sslopt-vars.h"
    "/home/huanghao/mysql-connector-c/include/sslopt-case.h"
    "/home/huanghao/mysql-connector-c/include/sql_common.h"
    "/home/huanghao/mysql-connector-c/include/keycache.h"
    "/home/huanghao/mysql-connector-c/include/m_ctype.h"
    "/home/huanghao/mysql-connector-c/include/my_compiler.h"
    "/home/huanghao/mysql-connector-c/include/mysql_com_server.h"
    "/home/huanghao/mysql-connector-c/include/my_byteorder.h"
    "/home/huanghao/mysql-connector-c/include/byte_order_generic.h"
    "/home/huanghao/mysql-connector-c/include/byte_order_generic_x86.h"
    "/home/huanghao/mysql-connector-c/include/little_endian.h"
    "/home/huanghao/mysql-connector-c/include/big_endian.h"
    "/home/huanghao/mysql-connector-c/include/mysql_version.h"
    "/home/huanghao/mysql-connector-c/include/my_config.h"
    "/home/huanghao/mysql-connector-c/include/mysqld_ername.h"
    "/home/huanghao/mysql-connector-c/include/mysqld_error.h"
    "/home/huanghao/mysql-connector-c/include/sql_state.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Development")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Development")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/mysql" TYPE DIRECTORY FILES "/home/huanghao/mysql-connector-c/include/mysql/" REGEX "/[^/]*\\.h$" REGEX "/psi\\_abi[^/]*$" EXCLUDE)
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Development")

