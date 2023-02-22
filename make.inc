####################################################################
#  SLICOT make include file for Intel Fortran compiler.            #
#  SLICOT, Release 5.8                          .\slicot\make.inc  #
#  Vasile Sima                                                     #
#  October 31, 1996.                                               #
#  Revised Dec. 1999; Jan. 2005; Feb. 2009, Aug. 2020, Dec. 2022,  #
#          Feb. 2023.                                              #
####################################################################
#
#  The root directory for the SLICOT files
#
HOME = c:\slicot
PLAT = _win64
#
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
FORTRAN  = ifort
OPTS     = /O2 /fp:source /4I8
NOOPT    = -optimize:0
LOADER   = $(FORTRAN)
LOADOPTS = $(SLICOTLIB) $(LPKAUXLIB) $(LAPACKLIB)
#
#  The archiver and the flag(s) to use when building archive (library)
#  If your system has no ranlib, set RANLIB = echo.
#
ARCH     = lib
ARCHFLAGS= -out
#ARCHFLAGS= 
RANLIB   = echo
#
#  The location of the libraries to which you will link.  (The 
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
# LAPACKLIB    = mkl_lapack.lib mkl_intel_c.lib mkl_sequential.lib mkl_core.lib
SLICOTLIB    = $(HOME)\slicot.lib
LPKAUXLIB    = $(HOME)\lpkaux.lib
LAPACKLIB    = mkl_intel_ilp64.lib mkl_sequential.lib mkl_core.lib
