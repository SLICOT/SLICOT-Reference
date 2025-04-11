cmake_minimum_required(VERSION 3.16)
project(SLICOT LANGUAGES Fortran)

# Version information
set(SLICOT_VERSION_MAJOR 5)
set(SLICOT_VERSION_MINOR 9)
set(SLICOT_VERSION "${SLICOT_VERSION_MAJOR}.${SLICOT_VERSION_MINOR}")

# Set options
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(SLICOT_USE_64BIT_INTEGERS "Use 64-bit integers" OFF)
option(SLICOT_BUILD_EXAMPLES "Build SLICOT examples" ON)
option(SLICOT_USE_VCPKG "Use VCPkg to find BLAS and LAPACK" OFF)

# Add option for vendor-specific BLAS/LAPACK
set(SLICOT_BLAS_VENDOR "Generic" CACHE STRING "BLAS/LAPACK vendor")
set_property(CACHE SLICOT_BLAS_VENDOR PROPERTY STRINGS
             "Generic" "Intel" "OpenBLAS" "Apple" "AMD" "ARM")

# Set default install directories
include(GNUInstallDirs)

# Configure Fortran compiler options
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    if(SLICOT_USE_64BIT_INTEGERS)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8")
    endif()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    if(SLICOT_USE_64BIT_INTEGERS)
        if(WIN32)
            set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /4I8")
        else()
            set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -integer-size 64")
        endif()
    endif()
    if(WIN32)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /fp:source")
        set(CMAKE_Fortran_FLAGS_RELEASE "/O2")
        set(CMAKE_Fortran_FLAGS_DEBUG "/Od /debug:full")
    else()
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
        set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang|LLVM")
    if(SLICOT_USE_64BIT_INTEGERS)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8")
    endif()
    set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")
endif()

# Find BLAS and LAPACK
if(SLICOT_USE_VCPKG)
    # VCPkg use
    find_package(BLAS CONFIG REQUIRED)
    find_package(LAPACK CONFIG REQUIRED)
    set(BLAS_LIBRARIES BLAS::BLAS)
    set(LAPACK_LIBRARIES LAPACK::LAPACK)
else()
    # Vendor-specific implementations
    if(SLICOT_BLAS_VENDOR STREQUAL "Intel")
        if(WIN32)
            # Intel MKL on Windows
            set(MKL_INTERFACE "ilp64" CACHE STRING "MKL interface type: lp64 or ilp64")
            find_library(MKL_INTERFACE_LIB mkl_intel_${MKL_INTERFACE})
            find_library(MKL_THREADING_LIB mkl_sequential)
            find_library(MKL_CORE_LIB mkl_core)
            set(BLAS_LIBRARIES ${MKL_INTERFACE_LIB} ${MKL_THREADING_LIB} ${MKL_CORE_LIB})
            set(LAPACK_LIBRARIES ${BLAS_LIBRARIES})
        else()
            # Intel MKL on Linux/macOS
            find_package(MKL QUIET)
            if(MKL_FOUND)
                set(BLAS_LIBRARIES ${MKL_LIBRARIES})
                set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
            else()
                message(STATUS "MKL not found via find_package, trying manual detection")
                find_path(MKL_INCLUDE_DIR mkl.h
                          PATHS $ENV{MKLROOT}/include)
                find_library(MKL_INTERFACE_LIB mkl_intel_ilp64
                             PATHS $ENV{MKLROOT}/lib $ENV{MKLROOT}/lib/intel64)
                find_library(MKL_THREADING_LIB mkl_sequential
                             PATHS $ENV{MKLROOT}/lib $ENV{MKLROOT}/lib/intel64)
                find_library(MKL_CORE_LIB mkl_core
                             PATHS $ENV{MKLROOT}/lib $ENV{MKLROOT}/lib/intel64)
                set(BLAS_LIBRARIES ${MKL_INTERFACE_LIB} ${MKL_THREADING_LIB} ${MKL_CORE_LIB})
                set(LAPACK_LIBRARIES ${BLAS_LIBRARIES})
            endif()
        endif()
    elseif(SLICOT_BLAS_VENDOR STREQUAL "OpenBLAS")
        # OpenBLAS
        find_package(OpenBLAS QUIET)
        if(OpenBLAS_FOUND)
            set(BLAS_LIBRARIES ${OpenBLAS_LIBRARIES})
        else()
            find_library(OPENBLAS_LIB openblas)
            set(BLAS_LIBRARIES ${OPENBLAS_LIB})
        endif()
        set(LAPACK_LIBRARIES ${BLAS_LIBRARIES})
    elseif(SLICOT_BLAS_VENDOR STREQUAL "AMD")
        # AMD AOCL
        find_library(AOCL_BLAS_LIB blis)
        find_library(AOCL_LAPACK_LIB flame)
        set(BLAS_LIBRARIES ${AOCL_BLAS_LIB})
        set(LAPACK_LIBRARIES ${AOCL_LAPACK_LIB} ${AOCL_BLAS_LIB})
    elseif(SLICOT_BLAS_VENDOR STREQUAL "ARM")
        # ARM Performance Libraries
        find_library(ARMPL_LIB armpl)
        set(BLAS_LIBRARIES ${ARMPL_LIB})
        set(LAPACK_LIBRARIES ${ARMPL_LIB})
    elseif(SLICOT_BLAS_VENDOR STREQUAL "Apple")
        # Apple Accelerate framework
        find_library(ACCELERATE_FRAMEWORK Accelerate)
        set(BLAS_LIBRARIES ${ACCELERATE_FRAMEWORK})
        set(LAPACK_LIBRARIES ${ACCELERATE_FRAMEWORK})
    else()
        # Generic BLAS/LAPACK
        find_package(BLAS REQUIRED)
        find_package(LAPACK REQUIRED)
    endif()
endif()

# Report found libraries
message(STATUS "BLAS libraries: ${BLAS_LIBRARIES}")
message(STATUS "LAPACK libraries: ${LAPACK_LIBRARIES}")

# Add subdirectories
add_subdirectory(src_aux)
add_subdirectory(src)

if(SLICOT_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Installation
install(TARGETS slicot lpkaux
        EXPORT SLICOTTargets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Install header files if any
# install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# Create export targets
install(EXPORT SLICOTTargets
        FILE SLICOTTargets.cmake
        NAMESPACE SLICOT::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/SLICOT)

# Create and install config files
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/SLICOTConfigVersion.cmake"
    VERSION ${SLICOT_VERSION}
    COMPATIBILITY SameMajorVersion)

configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/SLICOTConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/SLICOTConfig.cmake"
    @ONLY)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/SLICOTConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/SLICOTConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/SLICOT)
