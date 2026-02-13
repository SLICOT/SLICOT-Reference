if(NOT DEFINED CMAKE_INSTALL_MODULEDIR)
    if(NOT DEFINED Fortran_MODULE_NAME)
        set(Fortran_MODULE_NAME ${PROJECT_NAME})
    endif()

    if (Fortran_MODULE_NAME STREQUAL "")
        set(
            CMAKE_INSTALL_MODULEDIR
            "${CMAKE_INSTALL_INCLUDEDIR}"
            CACHE
            STRING
            "Directory in prefix to install generated module files"
            )
    else()
        set(
            CMAKE_INSTALL_MODULEDIR
            "${CMAKE_INSTALL_INCLUDEDIR}/${Fortran_MODULE_NAME}"
            CACHE
            STRING
            "Directory in prefix to install generated module files"
            )
    endif()
endif()

MESSAGE(STATUS "CMAKE_INSTALL_MODULEDIR: ${CMAKE_INSTALL_MODULEDIR}")
if (IS_ABSOLUTE ${CMAKE_INSTALL_MODULEDIR})
    set(CMAKE_INSTALL_FULL_MODULEDIR ${CMAKE_INSTALL_MODULEDIR})
else()
    # Handle special cases:
    # - CMAKE_INSTALL_PREFIX == /
    # - CMAKE_INSTALL_PREFIX == /usr
    # - CMAKE_INSTALL_PREFIX == /opt/...
    if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/")
        if (NOT "${CMAKE_INSTALL_MODULEDIR}" MATCHES "^usr/")
            set(MODULEDIR "usr/${CMAKE_INSTALL_MODULEDIR}")
        endif()
        set(CMAKE_INSTALL_FULL_MODULEDIR "/${MODULEDIR}")
    elseif("${CMAKE_INSTALL_PREFIX}" MATCHES "^/usr/?$")
        set(CMAKE_INSTALL_FULL_MODULEDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_MODULEDIR}")
    elseif("${CMAKE_INSTALL_PREFIX}" MATCHES "^/opt/" AND NOT "${CMAKE_INSTALL_PREFIX}" MATCHES "^/opt/homebrew/")
        set(CMAKE_INSTALL_FULL_MODULEDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_MODULEDIR}")
    else()
        set(CMAKE_INSTALL_FULL_MODULEDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_MODULEDIR}")
    endif()
endif()
if(NOT IS_ABSOLUTE "${CMAKE_INSTALL_MODULEDIR}")
    SET(CMAKE_INSTALL_FULL_MODULEDIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_MODULEDIR}")
else()
    SET(CMAKE_INSTALL_FULL_MODULEDIR "${CMAKE_INSTALL_MODULEDIR}")
endif()


