cmake_minimum_required(VERSION 3.24)
PROJECT(SLICOTTEST LANGUAGES C Fortran)

# Set the correct find_package name for SLICOT depending on the integer size
if (SLICOT_INTEGER8)
  set(SLICOT_NAME "SLICOT64")
else ()
  set(SLICOT_NAME "SLICOT")
endif()

include(FetchContent)
FetchContent_Declare(
  SLICOT
  GIT_REPOSITORY https://github.com/SLICOT/SLICOT-Reference.git
  GIT_TAG        main
    #  FIND_PACKAGE_ARGS NAMES ${SLICOT_NAME}
  OVERRIDE_FIND_PACKAGE
)

# This will try calling find_package() first for both dependencies
FetchContent_MakeAvailable(SLICOT)

ADD_EXECUTABLE(TAB01MD TAB01MD.f)
TARGET_LINK_LIBRARIES(TAB01MD SLICOT::slicot)
