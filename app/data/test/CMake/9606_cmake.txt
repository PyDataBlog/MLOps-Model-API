#----------------------------------------------------------------
# Generated CMake target import file for configuration "".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "FaceDetect" for configuration ""
set_property(TARGET FaceDetect APPEND PROPERTY IMPORTED_CONFIGURATIONS NOCONFIG)
set_target_properties(FaceDetect PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_NOCONFIG "pthread;omniORB4;omnithread;omniDynamic4;RTC;coil;opencv_videostab;opencv_video;opencv_ts;opencv_superres;opencv_stitching;opencv_photo;opencv_ocl;opencv_objdetect;opencv_nonfree;opencv_ml;opencv_legacy;opencv_imgproc;opencv_highgui;opencv_gpu;opencv_flann;opencv_features2d;opencv_core;opencv_contrib;opencv_calib3d;libmwmclmcrrt.so"
  IMPORTED_LOCATION_NOCONFIG "${_IMPORT_PREFIX}/components/lib/FaceDetect.so"
  IMPORTED_SONAME_NOCONFIG "FaceDetect.so"
  )

list(APPEND _IMPORT_CHECK_TARGETS FaceDetect )
list(APPEND _IMPORT_CHECK_FILES_FOR_FaceDetect "${_IMPORT_PREFIX}/components/lib/FaceDetect.so" )

# Import target "FaceDetectComp" for configuration ""
set_property(TARGET FaceDetectComp APPEND PROPERTY IMPORTED_CONFIGURATIONS NOCONFIG)
set_target_properties(FaceDetectComp PROPERTIES
  IMPORTED_LOCATION_NOCONFIG "${_IMPORT_PREFIX}/components/bin/FaceDetectComp"
  )

list(APPEND _IMPORT_CHECK_TARGETS FaceDetectComp )
list(APPEND _IMPORT_CHECK_FILES_FOR_FaceDetectComp "${_IMPORT_PREFIX}/components/bin/FaceDetectComp" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
