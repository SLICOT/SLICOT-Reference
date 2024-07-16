CMAKE_MINIMUM_REQUIRED(VERSION 3.15.0)
CMAKE_POLICY(SET CMP0048 NEW)
PROJECT(SLICOT VERSION 5.0.0 LANGUAGES C Fortran)
# Add addtional CMAKE Paths.
LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

INCLUDE(OutOfSource)

# Options
OPTION(BUILD_SHARED_LIBS "Build shared libraries" ON)
OPTION(DEBUG "Enable Debug Symbol generation" OFF)
OPTION(TESTING "Enabled Tests" ON)

# Enable PIC code by default
SET(CMAKE_POSITION_INDEPENDENT_CODE TRUE)

# Select the Proper Build Type.
IF ( DEBUG STREQUAL ON )
	SET (CMAKE_BUILD_TYPE "Debug")
	ADD_DEFINITIONS(-DDEBUG)
ENDIF()

IF(NOT CMAKE_BUILD_TYPE)
	SET (CMAKE_BUILD_TYPE "Release")
ENDIF()

IF (TESTING)
    ENABLE_TESTING()
ENDIF()

# Configure the Build

INCLUDE(FortranCompilerSettings)
INCLUDE(GNUInstallDirs)

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
ADD_SUBDIRECTORY(examples)
#  ADD_SUBDIRECTORY(test)
#
#  INCLUDE(CMakePackageConfigHelpers)
#  WRITE_BASIC_PACKAGE_VERSION_FILE(
#      qrupdateConfigVersion.cmake
#      VERSION ${PROJECT_VERSION}
#      COMPATIBILITY AnyNewerVersion
#   )
#  INSTALL(EXPORT qrupdatetargets
#          FILE qrupdateTargets.cmake
#          NAMESPACE qrupdate::
#          DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/qrupdate
#  )
#
#  CONFIGURE_FILE(qrupdate-config.cmake.in qrupdate-config.cmake @ONLY)
#  INSTALL(FILES "${CMAKE_CURRENT_BINARY_DIR}/qrupdate-config.cmake"
#                "${CMAKE_CURRENT_BINARY_DIR}/qrupdateConfigVersion.cmake"
#                DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/qrupdate
#          )
#
#  CONFIGURE_FILE(qrupdate.pc.in qrupdate.pc @ONLY)
#  INSTALL(FILES "${CMAKE_CURRENT_BINARY_DIR}/qrupdate.pc"
#                DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")
#
#  MESSAGE(STATUS "------------ Build Information --------------")
#  MESSAGE(STATUS "CMAKE_Fortran_FLAGS:  ${CMAKE_Fortran_FLAGS}")
#  MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG:  ${CMAKE_Fortran_FLAGS_DEBUG}")
#  MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE:  ${CMAKE_Fortran_FLAGS_RELEASE}")
#  MESSAGE(STATUS "BLAS Libraries: ${BLAS_LIBRARIES}")
#  MESSAGE(STATUS "LAPACK Libraries: ${LAPACK_LIBRARIES}")
#  MESSAGE(STATUS "Build Type: ${CMAKE_BUILD_TYPE}")
#  MESSAGE(STATUS "Debug: ${DEBUG}")
#  MESSAGE(STATUS "Shared Libs: ${BUILD_SHARED_LIBS}")
#  MESSAGE(STATUS "---------------------------------------------")
#
