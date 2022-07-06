# Install script for directory: /Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/Users/husseinalzandvirtual/linphone-iphone/liblinphone-sdk/x86_64-apple-darwin.ios")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ FILES
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3baserecognizer.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3basetree.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3basetreeadaptor.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3bitset.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3collections.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3commontoken.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3commontree.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3commontreeadaptor.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3commontreenodestream.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3convertutf.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3cyclicdfa.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3debugeventlistener.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3defs.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3encodings.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3errors.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3exception.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3filestream.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3input.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3interfaces.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3intstream.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3lexer.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3memory.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3parser.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3parsetree.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3recognizersharedstate.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3rewritestreams.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3string.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3tokenstream.h"
    "/Users/husseinalzandvirtual/linphone-iphone/submodules/externals/antlr3/runtime/C/include/antlr3treeparser.h"
    )
endif()

