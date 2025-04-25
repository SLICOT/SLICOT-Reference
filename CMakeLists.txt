CMAKE_MINIMUM_REQUIRED(VERSION 3.15.0)
CMAKE_POLICY(SET CMP0048 NEW)


# Adding C to the list of languages is necessary, such that BLAS
# libraries requiring C libraries as well will work. This for
# example is necessary to run MKL

PROJECT(SLICOT
    VERSION 5.9.0
    LANGUAGES C Fortran)

# Add addtional CMAKE Paths.
LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

# Enforce Out of Source Builds
INCLUDE(OutOfSource)

# Compile Options
OPTION(SLICOT_DEBUG "Enable Debug Symbol generation" OFF)
OPTION(SLICOT_INTEGER8 "Build with 64-bit integers (ILP64)" OFF) # Add ILP64 option

# Enable Testing
IF(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    OPTION(SLICOT_TESTING "Build the SLICOT example programs and tests" ON)
    IF (SLICOT_TESTING)
        INCLUDE(CTest)
        ENABLE_TESTING()
    ENDIF()
ELSE()
    SET(SLICOT_TESTING OFF)
ENDIF()



# Shared libraries
IF (SLICOT_INTEGER8)
    SET(INTEGER8 ON)
ELSE()
    SET(INTEGER8 OFF)
ENDIF()

# Enable PIC code by default
SET(CMAKE_POSITION_INDEPENDENT_CODE TRUE)
SET(CMAKE_EXPORT_COMPILE_COMMANDS TRUE)
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# Output directories
IF(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
   SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
   SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
   SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
ENDIF()

# Configure the Build

INCLUDE(FortranCompilerSettings)
INCLUDE(GNUInstallDirs)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Coverage")
endif()

# Select the Proper Build Type.
IF ( DEBUG STREQUAL ON OR CMAKE_BUILD_TYPE STREQUAL "Debug"  )
	SET (CMAKE_BUILD_TYPE "Debug")
	ADD_DEFINITIONS(-DDEBUG)
ENDIF()



# Coverage
set(_is_coverage_build 0)
set(_msg "Checking if build type is 'Coverage'")
message(STATUS "${_msg}")
if(NOT CMAKE_CONFIGURATION_TYPES)
  string(TOLOWER ${CMAKE_BUILD_TYPE} _build_type_lc)
  if(${_build_type_lc} STREQUAL "coverage")
    set(_is_coverage_build 1)
  endif()
endif()
message(STATUS "${_msg}: ${_is_coverage_build}")

if(_is_coverage_build)
  message(STATUS "Adding coverage")
  find_package(codecov)
endif()



# IBM XLF Compilation
IF( CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qfixed -qnosave")
ENDIF()


IF(INTEGER8)
    MESSAGE(STATUS "8-Byte Integer (ILP64) build.")
    IF(${CMAKE_VERSION} VERSION_LESS "3.22.0")
        MESSAGE(WARNING "8-Byte Integer (ILP64) build requires CMake 3.22 or newer to work properly.")
    ENDIF()
    SET(BLA_SIZEOF_INTEGER 8)
ELSE()
    SET(BLA_SIZEOF_INTEGER 4)
ENDIF()

FIND_PACKAGE(BLAS REQUIRED)
MESSAGE(STATUS "Found BLAS Library: ${BLA_VENDOR} (${BLAS_LIBRARIES})")
FIND_PACKAGE(LAPACK REQUIRED)
MESSAGE(STATUS "Found LAPACK Library: ${BLA_VENDOR} (${LAPACK_LIBRARIES})")

# Compatability with CMAKE < 3.18
if(NOT TARGET BLAS::BLAS)
    add_library(BLAS::BLAS IMPORTED INTERFACE)
    set_property(BLAS::BLAS PROPERTY INTERFACE_LINK_LIBRARIES ${BLAS_LIBRARIES})
    set_property(BLAS::BLAS PROPERTY INTERFACE_LINK_OPTIONS ${BLAS_LINKER_FLAGS})
endif()
if(NOT TARGET LAPACK::LAPACK)
    add_library(LAPACK::LAPACK IMPORTED INTERFACE)
    set_property(LAPACK::LAPACK PROPERTY INTERFACE_LINK_LIBRARIES ${LAPACK_LIBRARIES})
    set_property(LAPACK::LAPACK PROPERTY INTERFACE_LINK_OPTIONS ${LAPACK_LINKER_FLAGS})
endif()

ADD_SUBDIRECTORY(src)

IF(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR AND SLICOT_TESTING)
    ADD_SUBDIRECTORY(examples)
ENDIF()

# Configure the warning and code coverage suppression file
configure_file(
    "${PROJECT_SOURCE_DIR}/CTestCustom.cmake.in"
  "${PROJECT_BINARY_DIR}/CTestCustom.cmake"
  @ONLY
)



INCLUDE(CMakePackageConfigHelpers)
WRITE_BASIC_PACKAGE_VERSION_FILE(
    SLICOTConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion
 )
INSTALL(EXPORT slicot_targets
    FILE SLICOTTargets.cmake
    NAMESPACE SLICOT::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/SLICOT
)

CONFIGURE_FILE(SLICOTConfig.cmake.in SLICOTConfig.cmake @ONLY)
INSTALL(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/SLICOTConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/SLICOTConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/SLICOT
    )

CONFIGURE_FILE(slicot.pc.in slicot.pc @ONLY)
INSTALL(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/slicot.pc"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")

MESSAGE(STATUS "------------ Build Information --------------")
MESSAGE(STATUS "VERSION: ${PROJECT_VERSION} ${FULL_VERSION}")
MESSAGE(STATUS "CMAKE_Fortran_FLAGS:  ${CMAKE_Fortran_FLAGS}")
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG:  ${CMAKE_Fortran_FLAGS_DEBUG}")
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE:  ${CMAKE_Fortran_FLAGS_RELEASE}")
MESSAGE(STATUS "BLAS Libraries: ${BLAS_LIBRARIES}")
MESSAGE(STATUS "LAPACK Libraries: ${LAPACK_LIBRARIES}")
MESSAGE(STATUS "Build Type: ${CMAKE_BUILD_TYPE}")
MESSAGE(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
MESSAGE(STATUS "Shared Libs: ${BUILD_SHARED_LIBS}")
MESSAGE(STATUS "---------------------------------------------")

