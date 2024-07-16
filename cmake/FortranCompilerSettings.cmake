IF (CMAKE_Fortran_COMPILER_LOADED)

    OPTION(FORTRAN_BOUND_CHECK "Enable the Fortran bound checker" OFF)
    OPTION(FORTRAN_SANITIZE    "Enable the Fortran sanitizer" OFF)

    IF(NOT "${HOSTOPT}" STREQUAL "")
        IF(NOT (HOSTOPT STREQUAL OFF OR HOSTOPT STREQUAL ON))
            STRING(SUBSTRING "${HOSTOPT}" 0 1 FIRST_CHAR)
            IF( "${FIRST_CHAR}" STREQUAL "/" )
                MESSAGE(STATUS "Load user supplied Host Optimizations for Fortran -- ${HOSTOPT}")
                INCLUDE(${HOSTOPT})
            ELSE()
                MESSAGE(STATUS  "Load user supplied Host Optimizations for Fortran -- ${CMAKE_BINARY_DIR}/${HOSTOPT}")
                INCLUDE(${CMAKE_BINARY_DIR}/${HOSTOPT})
            ENDIF()
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${HOSTOPT_Fortran}" CACHE INTERNAL "")
        ENDIF()
    ENDIF()

    # INCLUDE(CheckFortranCompilerFlag_meta)
    INCLUDE(CheckFortranCompilerFlag)


    FUNCTION(ADD_FORTRAN_COMPILER_FLAG VAR FLAGNAME )
        IF(DEFINED CACHE{FORTRAN_${FLAGNAME}_WORK})
            RETURN()
        ENDIF()
        SET(_SAVE ${CMAKE_REQUIRED_QUIET})
        UNSET(_WORKS CACHE)
        SET(CMAKE_REQUIRED_QUIET TRUE)
        CHECK_FORTRAN_COMPILER_FLAG("${FLAGNAME}" _WORKS)
        SET(FORTRAN_${FLAGNAME}_WORK ${_WORKS} CACHE INTERNAL "Fortran Compiler supports ${FLAGNAME}")

        IF ( _WORKS)
            SET(${VAR} "${${VAR}} ${FLAGNAME}" CACHE INTERNAL "" )
            MESSAGE(STATUS "Fortran compiler supports ${FLAGNAME}")
        ELSE()
            MESSAGE(STATUS "Fortran compiler does not support ${FLAGNAME}")
        ENDIF()
        SET(CMAKE_REQUIRED_QUIET ${_SAVE})
    ENDFUNCTION()

    IF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        # GNU
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-frecursive")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fPIC")

        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_RELEASE "-O3")

        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wimplicit-procedure")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wall")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wunused")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Warray-temporaries")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-fbacktrace")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wshadow")

        IF(DEBUGOPT STREQUAL ON)
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O3")
        ELSE()
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O0")
        ENDIF()

        IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fcheck=bounds")
        ENDIF()

        IF ( FORTRAN_SANITIZE STREQUAL ON OR SANITIZE STREQUAL ON)
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fcheck=all")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=undefined")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=address")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=leak")
        ENDIF()

        IF(INTEGER8 STREQUAL ON)
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fdefault-integer-8")
        ENDIF()

        IF(HOSTOPT STREQUAL ON)
            IF (CMAKE_SYSTEM_PROCESSOR STREQUAL "ppc64le")
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-mcpu=native")
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-mtune=native")
            ELSE ()
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-march=native")
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-mtune=native")
            ENDIF()
        ENDIF()

        LIST(APPEND LIBRARIES "gfortran")

        SET(I8FLAG "-fdefault-integer-8")

    ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC -Mrecursive" CACHE INTERNAL "")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3" CACHE INTERNAL "")
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wimplicit-procedure -Wall -Wunused -Warray-temporaries -fbacktrace -Wshadow" CACHE INTERNAL "")

        IF(DEBUGOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O3" CACHE INTERNAL "")
        ELSE()
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0" CACHE INTERNAL "")
        ENDIF()

        IF(INTEGER8 STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8" CACHE INTERNAL "")
        ENDIF()

        IF(HOSTOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -march=native -mtune=native" CACHE INTERNAL "")
        ENDIF()

        SET(I8FLAG "-fdefault-integer-8")

    ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
        # Intel
        IF (WIN32)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /recursive /heap-arrays:64" CACHE INTERNAL "")
            SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} /O3 /Qunroll" CACHE INTERNAL "")
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /warn all /Zi /warn nointerfaces /traceback /debug all" CACHE INTERNAL "")

            IF(DEBUGOPT STREQUAL ON)
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "/O3")
            ELSE()
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "/O0")
            ENDIF()

            IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
                SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /check bounds" CACHE INTERNAL "")
            ENDIF()

            IF(HOSTOPT STREQUAL ON)
                SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /QxHost" CACHE INTERNAL "")
            ENDIF()

            IF(INTEGER8 STREQUAL ON)
                SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /4I8" CACHE INTERNAL "")
            ENDIF()

            LIST(APPEND LIBRARIES "ifcore")

            SET(I8FLAG "/4I8")
        ELSE()
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-recursive")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fpic")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-heap-arrays 64")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_RELEASE "-O3")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_RELEASE "-unroll")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-warn all")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-g")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-warn nointerfaces")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-traceback")
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-debug all")

            IF(DEBUGOPT STREQUAL ON)
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O3")
            ELSE()
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O0")
            ENDIF()

            IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-check bounds")
            ENDIF()

            IF(INTEGER8 STREQUAL ON)
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-i8")
            ENDIF()

            IF(HOSTOPT STREQUAL ON)
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-xHost")
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-flto")
                ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-ipo")
            ENDIF()
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-qopt-report=3")

            LIST(APPEND LIBRARIES "ifcore")

            SET(I8FLAG "-i8")
        ENDIF()

    ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC" OR CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
        # Nvidia HPC SDK (nvfortran) or PGI
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpic -Mrecursive -Mnoipa" CACHE INTERNAL "")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3" CACHE INTERNAL "")
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Minfo=all" CACHE INTERNAL "")

        IF(DEBUGOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -gopt -O3" CACHE INTERNAL "")
        ELSE()
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -O0" CACHE INTERNAL "")
        ENDIF()

        IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mbounds" CACHE INTERNAL "")
        ENDIF()

        IF(INTEGER8 STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8" CACHE INTERNAL "")
        ENDIF()

        IF(HOSTOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -fast -tp=native" CACHE INTERNAL "")
            IF (DEBUGOPT STREQUAL ON)
                SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fast -tp=native" CACHE INTERNAL "")
            ENDIF()
        ENDIF()

        SET(I8FLAG "-i8")

    ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
        # IBM XL
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qpic -qstrict=ieeefp -qnosave -qxlf77=nopersistent -qalias=std -qnoipa -qmaxmem=32768" CACHE INTERNAL "")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -qessl -qhot=level=2 -qreport -qlistfmt=html=all" CACHE INTERNAL "")
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -qsigtrap -g9" CACHE INTERNAL "")

        IF(HOSTOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O5 -qtune=auto -qarch=auto" CACHE INTERNAL "")
        ELSE()
            SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3" CACHE INTERNAL "")
        ENDIF()

        IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qcheck=all" CACHE INTERNAL "")
        ENDIF()

        IF(DEBUGOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O2" CACHE INTERNAL "")
        ELSE()
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0" CACHE INTERNAL "")
        ENDIF()

        STRING(REPLACE "-qhalt=e" "" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

        IF(INTEGER8 STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qintsize=8" CACHE INTERNAL "")
        ENDIF()

        IF(OPENMP_FOUND)
            LIST(REMOVE_ITEM CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "xlomp_ser")
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "")
        ENDIF()
        SET(I8FLAG "-qintsize=8")
    ENDIF()

ENDIF() # Compiler loaded
