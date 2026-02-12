Interaction with Build Systems
==============================

SLICOT can be integrated in software projects using different build systems.
Beside searching for the libraries files, as done by GNU Autotools, we support
the following.

**64-bit Integers:** If 64-bit integers are used, please ensure that the used
BLAS and LAPACK libraries are compatible with this. Furthermore, flags like
`-fdefault-integer-8` are required in the compilation process of your code.


CMAKE (after installation)
--------------------------

If SLICOT is built and installed using CMake, it installs a CMake package configuration
as well. Thus you just have to use
```cmake
find_package(SLICOT)
target_link_libraries(your_target PRIVATE SLICOT::slicot)
```
in your `CMakeLists.txt`. If the 64-bit integer build is required change the
find package command to `find_package(SLICOT64)`. If the 64-bit mode is used,
the `BLA_SIZEOF_INTEGER` variable for finding BLAS and LAPACK is set to 8. See
[FindBLAS](https://cmake.org/cmake/help/latest/module/FindBLAS.html) for details.

An example can be found in [`examples/cmake-how-to-example`](./examples/cmake-how-to-example)

pkg-config
----------

SLICOT installs [pkg-config](https://gitlab.freedesktop.org/pkg-config/pkg-config)
configuration files as well. The required compiler and linker flags can be
obtained via
```shell
pkg-config --cflags --libs slicot
```
or
```
pkg-config --cflags --libs slicot64
```
if you are using the 64-bit integer build.

CMAKE with FetchContent
-----------------------

Modern CMake allows to download, configure, and compile dependencies on demand.
This is also supported by SLICOT if CMake >= 3.24 is used. Inserting
```cmake
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
  GIT_TAG        main   # or a git tag, like a version, or a specific commit
  FIND_PACKAGE_ARGS NAMES ${SLICOT_NAME}
)

# This will try calling find_package() first otherwise downloads SLICOT
FetchContent_MakeAvailable(SLICOT)

target_link_libraries(your_target SLICOT::slicot)
```
in your `CMakeLists.txt` enables SLICOT in your project. This approach tries to
use an installed version of SLICOT first before downloading it. If the
downloaded version should be enforce replace
```
FIND_PACKAGE_ARGS NAMES ${SLICOT_NAME}
```
by
```
OVERRIDE_FIND_PACKAGE
```

An example can be found in [`examples/cmake-fetch-content`](./examples/cmake-fetch-content)


fpm - Fortran Packages Manager
------------------------------

SLICOT has experimental support for the
[Fortran Package Manager](https://fpm.fortran-lang.org). Just add the following
line to the dependencies in your `fpm.toml` file:
```
[dependencies]
slicot = { git = "https://github.com/SLICOT/SLICOT-Reference", tag = "v5.9.2" }
```
where the version needs to be at least `v5.9.2`. Otherwise your can use
```
[dependencies]
slicot = { git = "https://github.com/SLICOT/SLICOT-Reference", branch = "main" }
```
to use a specific branch or
```
[dependencies]
slicot = { git = "https://github.com/SLICOT/SLICOT-Reference", rev = "SHA" }
```
to include a special commit.

A minimal example is available in [`examples/fpm_example`](./examples/fpm_example).

**Attention:** The fpm support is experimental and no test runs are performed
due to limitations of the fpm build tool.

