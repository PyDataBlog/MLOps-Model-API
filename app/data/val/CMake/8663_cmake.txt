# Install script for directory: /home/chen/pcl-pcl-1.7.2/visualization

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

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FOREACH(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libpcl_visualization.so.1.7.2"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libpcl_visualization.so.1.7"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libpcl_visualization.so"
      )
    IF(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      FILE(RPATH_CHECK
           FILE "${file}"
           RPATH "/usr/local/lib")
    ENDIF()
  ENDFOREACH()
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
    "/home/chen/pcl-pcl-1.7.2/build/lib/libpcl_visualization.so.1.7.2"
    "/home/chen/pcl-pcl-1.7.2/build/lib/libpcl_visualization.so.1.7"
    "/home/chen/pcl-pcl-1.7.2/build/lib/libpcl_visualization.so"
    )
  FOREACH(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libpcl_visualization.so.1.7.2"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libpcl_visualization.so.1.7"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libpcl_visualization.so"
      )
    IF(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      FILE(RPATH_CHANGE
           FILE "${file}"
           OLD_RPATH "/home/chen/pcl-pcl-1.7.2/build/lib:"
           NEW_RPATH "/usr/local/lib")
      IF(CMAKE_INSTALL_DO_STRIP)
        EXECUTE_PROCESS(COMMAND "/usr/bin/strip" "${file}")
      ENDIF(CMAKE_INSTALL_DO_STRIP)
    ENDIF()
  ENDFOREACH()
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES "/home/chen/pcl-pcl-1.7.2/build/visualization/pcl_visualization-1.7.pc")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/pcl-1.7/pcl/visualization" TYPE FILE FILES
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/eigen.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/boost.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/cloud_viewer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/histogram_visualizer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/image_viewer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/interactor_style.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/pcl_visualizer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/pcl_painter2D.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/registration_visualizer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/point_cloud_handlers.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/point_cloud_color_handlers.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/point_cloud_geometry_handlers.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/keyboard_event.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/point_picking_event.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/area_picking_event.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/mouse_event.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/window.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/range_image_visualizer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/interactor.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/vtk.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/simple_buffer_visualizer.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/pcl_plotter.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/pcl-1.7/pcl/visualization/common" TYPE FILE FILES
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/actor_map.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/common.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/io.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/ren_win_interact_map.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/shapes.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/float_image_utils.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/pcl-1.7/pcl/visualization/impl" TYPE FILE FILES
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/histogram_visualizer.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/pcl_visualizer.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/image_viewer.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/registration_visualizer.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/point_cloud_handlers.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/point_cloud_color_handlers.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/point_cloud_geometry_handlers.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/impl/pcl_plotter.hpp"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/pcl-1.7/pcl/visualization/common/impl" TYPE FILE FILES
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/impl/shapes.hpp"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/common/impl/common.hpp"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/pcl-1.7/pcl/visualization/vtk" TYPE FILE FILES
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/vtk/pcl_image_canvas_source_2d.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/vtk/pcl_context_item.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/vtk/vtkVertexBufferObject.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/vtk/vtkRenderWindowInteractorFix.h"
    "/home/chen/pcl-pcl-1.7.2/visualization/include/pcl/visualization/vtk/vtkVertexBufferObjectMapper.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "pcl_visualization")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/home/chen/pcl-pcl-1.7.2/build/visualization/tools/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

