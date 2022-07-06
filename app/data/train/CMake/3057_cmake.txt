#----------------------------------------------------------------
# Generated CMake target import file for configuration "MinSizeRel".
#----------------------------------------------------------------

# Commands may need to know the format version.
SET(CMAKE_IMPORT_FILE_VERSION 1)

# Compute the installation prefix relative to this file.
GET_FILENAME_COMPONENT(_IMPORT_PREFIX "${CMAKE_CURRENT_LIST_FILE}" PATH)
GET_FILENAME_COMPONENT(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
GET_FILENAME_COMPONENT(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmCommon" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmCommon APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmCommon PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "ws2_32"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmCommon.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmCommon )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmCommon "${_IMPORT_PREFIX}/lib/gdcmCommon.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmDICT" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmDICT APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmDICT PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "gdcmDSED;gdcmIOD"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmDICT.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmDICT )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmDICT "${_IMPORT_PREFIX}/lib/gdcmDICT.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmDSED" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmDSED APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmDSED PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "gdcmCommon;gdcmzlib"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmDSED.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmDSED )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmDSED "${_IMPORT_PREFIX}/lib/gdcmDSED.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmIOD" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmIOD APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmIOD PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "gdcmDSED;gdcmCommon;gdcmexpat"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmIOD.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmIOD )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmIOD "${_IMPORT_PREFIX}/lib/gdcmIOD.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmMSFF" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmMSFF APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmMSFF PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "gdcmIOD;gdcmDSED;gdcmDICT;gdcmjpeg8;gdcmjpeg12;gdcmjpeg16;gdcmopenjpeg;gdcmcharls;rpcrt4"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmMSFF.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmMSFF )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmMSFF "${_IMPORT_PREFIX}/lib/gdcmMSFF.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmMEXD" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmMEXD APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmMEXD PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "gdcmMSFF;gdcmDICT;gdcmDSED;gdcmIOD;socketxx;ws2_32"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmMEXD.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmMEXD )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmMEXD "${_IMPORT_PREFIX}/lib/gdcmMEXD.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmjpeg8" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmjpeg8 APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmjpeg8 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "C"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmjpeg8.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmjpeg8 )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmjpeg8 "${_IMPORT_PREFIX}/lib/gdcmjpeg8.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmjpeg12" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmjpeg12 APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmjpeg12 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "C"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmjpeg12.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmjpeg12 )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmjpeg12 "${_IMPORT_PREFIX}/lib/gdcmjpeg12.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmjpeg16" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmjpeg16 APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmjpeg16 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "C"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmjpeg16.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmjpeg16 )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmjpeg16 "${_IMPORT_PREFIX}/lib/gdcmjpeg16.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmexpat" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmexpat APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmexpat PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "C"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmexpat.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmexpat )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmexpat "${_IMPORT_PREFIX}/lib/gdcmexpat.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmopenjpeg" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmopenjpeg APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmopenjpeg PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "C"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmopenjpeg.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmopenjpeg )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmopenjpeg "${_IMPORT_PREFIX}/lib/gdcmopenjpeg.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmcharls" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmcharls APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmcharls PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmcharls.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmcharls )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmcharls "${_IMPORT_PREFIX}/lib/gdcmcharls.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "gdcmzlib" for configuration "MinSizeRel"
SET_PROPERTY(TARGET gdcmzlib APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(gdcmzlib PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "C"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/gdcmzlib.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS gdcmzlib )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_gdcmzlib "${_IMPORT_PREFIX}/lib/gdcmzlib.lib" )

# Make sure the targets which have been exported in some other 
# export set exist.

# Import target "socketxx" for configuration "MinSizeRel"
SET_PROPERTY(TARGET socketxx APPEND PROPERTY IMPORTED_CONFIGURATIONS MINSIZEREL)
SET_TARGET_PROPERTIES(socketxx PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_MINSIZEREL "CXX"
  IMPORTED_LINK_INTERFACE_LIBRARIES_MINSIZEREL "ws2_32.lib"
  IMPORTED_LOCATION_MINSIZEREL "${_IMPORT_PREFIX}/lib/socketxx.lib"
  )

LIST(APPEND _IMPORT_CHECK_TARGETS socketxx )
LIST(APPEND _IMPORT_CHECK_FILES_FOR_socketxx "${_IMPORT_PREFIX}/lib/socketxx.lib" )

# Loop over all imported files and verify that they actually exist
FOREACH(target ${_IMPORT_CHECK_TARGETS} )
  FOREACH(file ${_IMPORT_CHECK_FILES_FOR_${target}} )
    IF(NOT EXISTS "${file}" )
      MESSAGE(FATAL_ERROR "The imported target \"${target}\" references the file
   \"${file}\"
but this file does not exist.  Possible reasons include:
* The file was deleted, renamed, or moved to another location.
* An install or uninstall procedure did not complete successfully.
* The installation package was faulty and contained
   \"${CMAKE_CURRENT_LIST_FILE}\"
but not all the files it references.
")
    ENDIF()
  ENDFOREACH()
  UNSET(_IMPORT_CHECK_FILES_FOR_${target})
ENDFOREACH()
UNSET(_IMPORT_CHECK_TARGETS)

# Cleanup temporary variables.
SET(_IMPORT_PREFIX)

# Commands beyond this point should not need to know the version.
SET(CMAKE_IMPORT_FILE_VERSION)
