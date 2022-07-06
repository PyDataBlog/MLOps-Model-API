FIND_PACKAGE(fitsio REQUIRED)

if ("$ENV{HOSTNAME}" STREQUAL "wasphead")
    FIND_PATH(CCFITS_INCLUDE_DIR CCfits/CCfits PATHS 
        $ENV{HOME}/build/include
        NO_DEFAULT_PATH
        #/usr/local/include
        )
    FIND_LIBRARY(CCFITS_LIBRARIES NAMES CCfits PATHS
        $ENV{HOME}/build/lib64
        $ENV{HOME}/build/lib
        #/usr/local/lib
        )
else()
    FIND_PATH(CCFITS_INCLUDE_DIR CCfits/CCfits PATHS 
        $ENV{HOME}/build/include
        #NO_DEFAULT_PATH
        /usr/local/include
        )
    
    FIND_LIBRARY(CCFITS_LIBRARIES NAMES CCfits PATHS
        $ENV{HOME}/build/lib64
        $ENV{HOME}/build/lib
        /usr/local/lib
        )
endif()



if (CCFITS_INCLUDE_DIR AND CCFITS_LIBRARIES AND FITSIO_INCLUDE_DIR AND FITSIO_LIBRARIES)
    set (CCFITS_FOUND true)
endif(CCFITS_INCLUDE_DIR AND CCFITS_LIBRARIES AND FITSIO_INCLUDE_DIR AND FITSIO_LIBRARIES)


if (CCFITS_FOUND)
    set(CCFITS_INCLUDE_DIR "${CCFITS_INCLUDE_DIR};${FITSIO_INCLUDE_DIR}")
    set(CCFITS_LIBRARIES "${CCFITS_LIBRARIES};${FITSIO_LIBRARIES}")
    if (NOT CCFITS_FIND_QUIETLY)
        message(STATUS "Found CCfits: ${CCFITS_LIBRARIES}")
    endif(NOT CCFITS_FIND_QUIETLY)
else (CCFITS_FOUND)
   if (CCFITS_FIND_REQUIRED)
       message(FATAL_ERROR "Could not find CCfits")
    endif(CCFITS_FIND_REQUIRED)
endif(CCFITS_FOUND) 
