# The set of languages for which implicit dependencies are needed:
SET(CMAKE_DEPENDS_LANGUAGES
  "CXX"
  )
# The set of files for implicit dependencies of each language:
SET(CMAKE_DEPENDS_CHECK_CXX
  "/opt/crtmpserver/sources/applications/vptests/src/rtmpappprotocolhandler.cpp" "/opt/crtmpserver/builders/cmake/applications/vptests/CMakeFiles/vptests.dir/opt/crtmpserver/sources/applications/vptests/src/rtmpappprotocolhandler.cpp.o"
  "/opt/crtmpserver/sources/applications/vptests/src/variantappprotocolhandler.cpp" "/opt/crtmpserver/builders/cmake/applications/vptests/CMakeFiles/vptests.dir/opt/crtmpserver/sources/applications/vptests/src/variantappprotocolhandler.cpp.o"
  "/opt/crtmpserver/sources/applications/vptests/src/vptests.cpp" "/opt/crtmpserver/builders/cmake/applications/vptests/CMakeFiles/vptests.dir/opt/crtmpserver/sources/applications/vptests/src/vptests.cpp.o"
  "/opt/crtmpserver/sources/applications/vptests/src/vptestsapplication.cpp" "/opt/crtmpserver/builders/cmake/applications/vptests/CMakeFiles/vptests.dir/opt/crtmpserver/sources/applications/vptests/src/vptestsapplication.cpp.o"
  )
SET(CMAKE_CXX_COMPILER_ID "GNU")

# Preprocessor definitions for this target.
SET(CMAKE_TARGET_DEFINITIONS
  "CreateRTCPPacket=CreateRTCPPacket_mystyle_only_once"
  "FeedDataAudioMPEG4Generic=FeedDataAudioMPEG4Generic_one_by_one"
  "HAS_LUA"
  "HAS_MEDIA_FLV"
  "HAS_MEDIA_MP3"
  "HAS_MEDIA_MP4"
  "HAS_PROTOCOL_CLI"
  "HAS_PROTOCOL_HLS"
  "HAS_PROTOCOL_HTTP"
  "HAS_PROTOCOL_LIVEFLV"
  "HAS_PROTOCOL_RTMP"
  "HAS_PROTOCOL_RTP"
  "HAS_PROTOCOL_TS"
  "HAS_PROTOCOL_VAR"
  "HandleTSVideoData=HandleVideoData_version3"
  "LINUX"
  "LITTLE_ENDIAN_BYTE_ALIGNED"
  "NET_EPOLL"
  "_FILE_OFFSET_BITS=64"
  "__USE_FILE_OFFSET64"
  )

# Targets to which this target links.
SET(CMAKE_TARGET_LINKED_INFO_FILES
  "/opt/crtmpserver/builders/cmake/thelib/CMakeFiles/thelib.dir/DependInfo.cmake"
  "/opt/crtmpserver/builders/cmake/common/CMakeFiles/common.dir/DependInfo.cmake"
  "/opt/crtmpserver/builders/cmake/lua/CMakeFiles/lua.dir/DependInfo.cmake"
  "/opt/crtmpserver/builders/cmake/tinyxml/CMakeFiles/tinyxml.dir/DependInfo.cmake"
  )

# The include file search paths:
SET(CMAKE_C_TARGET_INCLUDE_PATH
  "/opt/crtmpserver/3rdparty/lua-dev"
  "/opt/crtmpserver/sources/common/include"
  "/opt/crtmpserver/sources/thelib/include"
  "/opt/crtmpserver/sources/applications/vptests/include"
  )
SET(CMAKE_CXX_TARGET_INCLUDE_PATH ${CMAKE_C_TARGET_INCLUDE_PATH})
SET(CMAKE_Fortran_TARGET_INCLUDE_PATH ${CMAKE_C_TARGET_INCLUDE_PATH})
SET(CMAKE_ASM_TARGET_INCLUDE_PATH ${CMAKE_C_TARGET_INCLUDE_PATH})
