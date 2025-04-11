# CMakeLists.txt for SLICOT examples

# Automatically find all example source files
file(GLOB EXAMPLE_SOURCES "*.f")

# Create an executable for each example
foreach(EXAMPLE_SOURCE ${EXAMPLE_SOURCES})
    # Get the filename without extension
    get_filename_component(EXAMPLE_NAME ${EXAMPLE_SOURCE} NAME_WE)
    
    # Create the executable
    add_executable(${EXAMPLE_NAME} ${EXAMPLE_SOURCE})
    
    # Link with slicot and other libraries
    target_link_libraries(${EXAMPLE_NAME} PRIVATE slicot lpkaux ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
    
    # Set output directory for all example executables
    set_target_properties(${EXAMPLE_NAME} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/examples")
endforeach()

# Copy example data files to the build directory
file(GLOB EXAMPLE_DATA_FILES "*.dat" "*.res")
foreach(DATA_FILE ${EXAMPLE_DATA_FILES})
    get_filename_component(FILENAME ${DATA_FILE} NAME)
    configure_file(${DATA_FILE} "${CMAKE_BINARY_DIR}/bin/examples/${FILENAME}" COPYONLY)
endforeach()

# Create a custom target to run all examples
add_custom_target(run_examples
    COMMAND ${CMAKE_COMMAND} -E echo "Running all SLICOT examples"
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/bin/examples"
)

# Add commands to run each example and capture output to .exa files
foreach(EXAMPLE_SOURCE ${EXAMPLE_SOURCES})
    get_filename_component(EXAMPLE_NAME ${EXAMPLE_SOURCE} NAME_WE)
    add_custom_command(
        TARGET run_examples
        COMMAND ${CMAKE_COMMAND} -E echo "Running ${EXAMPLE_NAME}"
        COMMAND ${CMAKE_BINARY_DIR}/bin/examples/${EXAMPLE_NAME} > ${EXAMPLE_NAME}.exa
        COMMAND ${CMAKE_COMMAND} -E echo "${EXAMPLE_NAME} completed"
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/bin/examples"
    )
endforeach()
