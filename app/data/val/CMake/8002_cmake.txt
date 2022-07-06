#  SHIBOKEN_INCLUDE_DIR        - Directories to include to use SHIBOKEN
#  SHIBOKEN_LIBRARY            - Files to link against to use SHIBOKEN
#  SHIBOKEN_BINARY             - Executable name
#  SHIBOKEN_BUILD_TYPE         - Tells if Shiboken was compiled in Release or Debug mode.
#  SHIBOKEN_PYTHON_INTERPRETER - Python interpreter (regular or debug) to be used with the bindings.
#  SHIBOKEN_PYTHON_LIBRARIES   - Python libraries (regular or debug) Shiboken is linked against.

SET(SHIBOKEN_INCLUDE_DIR "/tmp/pyside-setup-tmp/pyside_install/py2.6-qt4.8.6-64bit-release/include/shiboken")
if(MSVC)
    SET(SHIBOKEN_LIBRARY "/tmp/pyside-setup-tmp/pyside_install/py2.6-qt4.8.6-64bit-release/lib/libshiboken-python2.6.lib")
elseif(CYGWIN)
    SET(SHIBOKEN_LIBRARY "/tmp/pyside-setup-tmp/pyside_install/py2.6-qt4.8.6-64bit-release/lib/shiboken-python2.6")
elseif(WIN32)
    SET(SHIBOKEN_LIBRARY "/tmp/pyside-setup-tmp/pyside_install/py2.6-qt4.8.6-64bit-release/bin/libshiboken-python2.6.so")
else()
    SET(SHIBOKEN_LIBRARY "/tmp/pyside-setup-tmp/pyside_install/py2.6-qt4.8.6-64bit-release/lib/libshiboken-python2.6.so")
endif()
SET(SHIBOKEN_PYTHON_INCLUDE_DIR "/usr/include/python2.6")
SET(SHIBOKEN_PYTHON_INCLUDE_DIR "/usr/include/python2.6")
SET(SHIBOKEN_PYTHON_INTERPRETER "/usr/bin/python2.6")
SET(SHIBOKEN_PYTHON_LIBRARIES "/usr/lib64/libpython2.6.so")
SET(SHIBOKEN_PYTHON_SUFFIX "-python2.6")
message(STATUS "libshiboken built for Release")


set(SHIBOKEN_BINARY "/tmp/pyside-setup-tmp/pyside_install/py2.6-qt4.8.6-64bit-release/bin/shiboken")
