# This script fetches and builds VolumeViewer

SET(VolumeViewer_ROOT
  "${CMAKE_BINARY_DIR}/VolumeViewer"
  )

SET(VolumeViewer_CMAKE_ARGS
  -DVRUI_PKGCONFIG_DIR:PATH=${VRUI_ROOT}/pkgconfig
  -DVTK_DIR:PATH=${VTK_ROOT}/bld
  -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_BINARY_DIR}
  )

ExternalProject_Add(
  VolumeViewer
  DEPENDS VRUI VTK
  PREFIX "${VolumeViewer_ROOT}"
  STAMP_DIR "${VolumeViewer_ROOT}/stamp"
  GIT_REPOSITORY "https://github.com/VruiVTK/VolumeViewer.git"
  GIT_TAG "master"
  #  UPDATE_COMMAND ${GIT_EXECUTABLE} pull
  SOURCE_DIR "${VolumeViewer_ROOT}/src"
  BINARY_DIR "${VolumeViewer_ROOT}/bld"
  CMAKE_ARGS ${VolumeViewer_CMAKE_ARGS}
  )

