# CMakeLists.txt for SLICOT main sources (src)

# Get all Fortran source files in the directory
file(GLOB SLICOT_SOURCES "*.f")

# Create the slicot library
add_library(slicot ${SLICOT_SOURCES})

# Set library properties
set_target_properties(slicot PROPERTIES
    VERSION ${SLICOT_VERSION}
    SOVERSION ${SLICOT_VERSION_MAJOR}
    POSITION_INDEPENDENT_CODE ON)

# Add include directories if needed
# target_include_directories(slicot PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Link with BLAS, LAPACK, and the auxiliary library
target_link_libraries(slicot 
    PUBLIC 
        ${BLAS_LIBRARIES}
        ${LAPACK_LIBRARIES} 
    PRIVATE 
        lpkaux)

# Ensure lpkaux is built first
add_dependencies(slicot lpkaux)
