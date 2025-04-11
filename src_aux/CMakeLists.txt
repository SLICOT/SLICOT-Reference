# CMakeLists.txt for SLICOT auxiliary sources (src_aux)
# This contains deprecated LAPACK routines needed by SLICOT

# Get all Fortran source files in the directory
file(GLOB SRC_AUX_SOURCES "*.f")

# Create the lpkaux library
add_library(lpkaux ${SRC_AUX_SOURCES})

# Set library properties
set_target_properties(lpkaux PROPERTIES
    VERSION ${SLICOT_VERSION}
    SOVERSION ${SLICOT_VERSION_MAJOR}
    POSITION_INDEPENDENT_CODE ON)

# Add include directories if needed
# target_include_directories(lpkaux PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Link with BLAS and LAPACK
target_link_libraries(lpkaux PUBLIC ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
