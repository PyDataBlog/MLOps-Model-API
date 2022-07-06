
string(COMPARE EQUAL "$ENV{NDK_ROOT}" "" __empty_ndk_root_env)
if(__empty_ndk_root_env)
  message(FATAL_ERROR "No NDK_ROOT supplied")
endif()

set(ANDROID_ABI x86_64)

set(ANDROID_PLATFORM 21)

include($ENV{NDK_ROOT}/build/cmake/android.toolchain.cmake)

set(CMAKE_CXX_STANDARD 14) # Override the default

