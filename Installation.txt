# SLICOT Software Installation and Updating

This file describes how to install and update the **SLICOT** Library, and how to run the example programs.

The essential source code and documentation for the **SLICOT** software is stored in the **SLICOT** Library root directory, **`slicot`**, and its subdirectories (**`benchmark_data`**, **`doc`**, **`examples`**, **`src`** and **`src_aux`**).

The object and/or executable files can be built using the information given in this file.  **SLICOT** routines make calls to subprograms from the state-of-the-art packages **LAPACK** (Linear Algebra Package) and **BLAS** (Basic Linear Algebra Subprograms).  Fortran source code and prebuilt, Fortran-based **LAPACK** and **BLAS** libraries are freely downloadable from https://netlib.org.  However, for maximum efficiency it is recommended to use machine-specific, optimized versions whenever possible.

Template make files are provided to help building the **SLICOT** Library object file, and to link and run the available example programs calling the **SLICOT** Library routines.  In order to use these make files on a specific Unix-like or Windows platform, some changes might be needed in the files **`make*.inc`** and **`makefile*`** stored in the **SLICOT** (sub-)directories, **`slicot`**, **`examples`**, **`src`** and **`src_aux`**.  The file named **`make.inc`** and the files **`makefile`** have been used on Windows platforms with Intel Fortran compilers.  The files named **`make_Unix.inc`** and **`makefile_Unix`** are templates for Unix-like machines, including Linux, with gfortran compiler.  Denote by <**slicotroot**> the path to the **`slicot`** directory, which can be, e.g., **`c:\slicot`**, on Windows platforms.  (The last (sub)directory name in <**slicotroot**> is **`slicot`**.)

The changes in **`make*.inc`** might define the specific machine (platform) identifier, the compiler, linker, and archiver flags, and the location and names of the **LAPACK** and **BLAS** libraries, which the program files should be linked to.  Some details are given in the **`make*.inc`** files.

After performing the necessary changes, as suggested in the comments of the make files, the other needed **SLICOT**-related files can be generated automatically with the command

**`make`**   (or **`nmake`**, for Windows platforms)

issued from the directory **`slicot`** of <**slicotroot**>.

The first execution of **`(n)make`** will create the following files

  - the **SLICOT** Library object files **`*.o`** (for Unix machines), or **`*.obj`** (for Windows machines), in the subdirectories **`src`** and **`src_aux`** of **`slicot`**;
  - the library files **`slicot.a`** and **`lpkaux.a`**, or **`slicot.lib`** and **`lpkaux.lib`**, respectively, in the directory **`slicot`**; the libraries **`lpkaux.a`** or **`lpkaux.lib`** contain the object files for two **LAPACK** deprecated subroutines, **`dlatzm.f`** and **`zlatzm.f`**, which are called by few **SLICOT** routines.  
  - the example programs object and executable files, in the subdirectory **`examples`**;
  - the files **`*.exa`**, with the results computed on the local machine, with the same name as for the files with data (**`*.dat`**) and reference results (**`*.res`**), also in the subdirectory **`examples`**.

The subsequent executions of **`(n)make`** will update the files if changes have been performed (assuming that the object and executable files have been preserved on their subdirectories).

The files **`*.exa`**, with the computed results may be compared with the reference results.  Several types of differences could be noticed, including possible sign changes for some elements, or even different values in some columns and/or rows of the computed matrices.  For instance, the matrices of similarity or equivalence transformations could differ from a platform/compiler to another.  This does not usually mean that the computed results are wrong.

More details for executing other tasks, e.g., cleaning the subdirectories **`src`** and **`examples`**, are given in the files **`makefile* included in directory **`slicot`** and in the subdirectories **`src`** and **`examples`**.

