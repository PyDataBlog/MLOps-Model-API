find_package(Qt4 REQUIRED QtCore QtGui )

include(${QT_USE_FILE})
include_directories(${CMAKE_BINARY_DIR} ${QT_INCLUDES})

set(DEPS_LIBS ${QT_LIBRARIES})
