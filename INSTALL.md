SLICOT Software Installation
============================

This file describes how to install and update the **SLICOT** Library, and how
to run the example programs.

Requirements
------------

In order to build SLICOT you need:

 * a Fortran 77 compatible Fortran compiler and a corresponding C compiler
 * cmake, at least 3.15 or 3.22 for the ILP64 build
 * a BLAS and LAPACK library, preferable an optimized one like OpenBLAS

The following compilers  are tested

 * gcc/gfortran
 * clang/flang
 * Intel Classic icc/ifort
 * Intel LLVM icx/ifx
 * Nvidia HPC SDK >= 21.11
 * AMD AOCC 5.0 with AMDBLIS and AMDLIBFLAME

We support Linux, xBSD, MacOSX (Intel and ARM), Windows with MSYS2.

For performance reasons, we suggest to use an optimized BLAS implementation
like

 * OpenBLAS, https://www.openblas.net
 * BLIS, https://github.com/flame/blis
 * Intel oneAPI MKL, https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl.html
 * FlexiBLAS, https://www.mpi-magdeburg.mpg.de/projects/flexiblas

Installation
------------
SLICOT requires an *out-of-source* build process. That means, the source code
direcotry stays untouched during the build.

The in order to configure and compile the source, use `cmake`:
```shell
cmake -S . -B build-dir <OPTIONS>
cmake --build build-dir
```
after building, SLICOT can be installed via
```shell
cmake --build build-dir --target install
```

The test suite is executed using
```shell
ctest --test-dir build-dir
```

The following options to cmake are availble:

| Option                 | Possible Values | Default Value | Description        |
|:-----------------------|:----------------|:--------------|--------------------|
|`CMAKE_INSTALL_PREFIX`  | valid path      | `/usr/local ` | Installation path  |
|`CMAKE_BUILD_TYPE`      | `Debug`, `Release`, `MinSizeRel`, `RelWithDebInfo`, `Coverage` | `Release` | Specify the build type |
|`BUILD_SHARED_LIBS`     | `ON`, `OFF`     | `OFF`         | Build SLICOT as shared library |
|`SLICOT_TESTING`        | `ON`, `OFF`     | `ON`          | Build the examples and the test suite |
|`SLICOT_DEBUG`          | `ON`, `OFF`     | `OFF`         | Enable the debug build, equivalent to `CMAKE_BUILD_TYPE=Debug`|
|`SLICOT_INTEGER8`       | `ON`, `OFF`     | `OFF`         | Enable the ILP64 integer model, i.e. the Fortran `INTEGER` defaults to a 64-bit integer, requires cmake >= 3.22|
|`SLICOT_STATIC_WITH_PIC`| `ON`, `OFF`     | `ON`          | Build static library with `-fPIC`, default for shared libraries |
|`BLA_VENDOR`            | BLAS Vendor Name | empty        | Specify the BLAS library to search for, see https://cmake.org/cmake/help/latest/module/FindBLAS.html for details |

The options are passed as `-DOPTION=VALUE` to cmake.

Windows
-------

Regarding Microsoft Windows, we only support the MSYS2 environment at the
moment. In order to build SLICOT, the following packages need to be installed in
advance:

 * cmake
 * ninja
 * lapack

Either install the corresponding version package, depending on your compiler,
yourself or use the `pacboy` package from the `pactoys`:

```shell
pacboy sync gcc-fortran cmake ninja lapack
```

The remaining build process works as described above.


RPM Packages
------------
On RHEL, its derivatives, and Fedora Linux, we provide RPM packaging as well. In
order to use this, install the following packages:
```shell
sudo dnf install cmake make gcc gcc-gfortran flexiblas-devel rpmdevtools
```
Instead `flexiblas-devel` also the reference packages can be installed. In this
case uninstall `flexiblas-devel` and install `blas-devel` and `lapack-devel`.

Afterwards, run
```shell
bash ./dist/make_rpm.sh
```
from the root of the source tree. This builds the SRPM and the RPM files for
SLICOT. The results can be found in `~/rpmbuild/SRPMS` and
`~/rpmbuild/RPMS/<ARCH>`. If only the SRPM is required, use
```shell
bash ./dist/make_rpm.sh -ts
```
instead.

Debian/Ubuntu Packages
----------------------
On Debian, Ubuntu and their derivatives packages can be built as well. This
requires the installation of then prerequisites:
```shell
sudo apt install cmake libblas-dev libblas64-dev liblapack-dev liblapack64-dev\
         git build-essential gfortran clang flang  ninja-build debhelper
```
and `debhelper` must be available in version 13.0. (This disables building
packages on Ubuntu before 22.04.)

After installing the requirements, the packages are bulild using
```shell
dpkg-buildpackage -uc -us
```


Issues
------
A list of known issues can be found in [KNOWN_ISSUES.md](./KNOWN_ISSUES.md)

Deprecation Warning
-------------------
**SLICOT could still by built by calling `make` and setting the proper values
in `make.inc`. This way is deprecated and will be removed in the next non-bugfix
release of SLICOT. Most-likely in 5.10 or 6.0.**
