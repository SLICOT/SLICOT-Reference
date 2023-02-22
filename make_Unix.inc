####################################################################
#  SLICOT make include file for Unix-like machines.                #
#  SLICOT, Release v5.8                           slicot/make.inc  #
#  Vasile Sima                                                     #
#  October 31, 1996.                                               #
#  Revised Dec. 7, 1999; July 16, 2015, Dec. 28, 2022, Feb. 2023.  #
####################################################################

SHELL = /bin/sh

#  Modify the FC and FFLAGS definitions to the desired compiler
#  and desired compiler options for your machine.  NOOPT refers to
#  the compiler options desired when NO OPTIMIZATION is selected.
#
FC = gfortran
FFLAGS = -O2 -fPIC -fdefault-integer-8
FFLAGS_NOOPT = -O0

#  Define LDFLAGS to the desired linker options for your machine.
#
LDFLAGS =

#  The archiver and the flag(s) to use when building an archive
#  (library).  If your system has no ranlib, set RANLIB = echo.
#
ARCH = ar
ARFLAGS = cr
# ARCHFLAGS= r
# RANLIB   = ranlib

LOADOPTS = $(SLICOTLIB) $(LPKAUXLIB) $(LAPACKLIB) $(BLASLIB)
#
#  The location of the libraries to which you will link.  (The
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
BLASLIB      = ../libs/blas.a
LAPACKLIB    = ../libs/lapack.a
LPKAUXLIB    = ../lpkaux.a
SLICOTLIB    = ../slicot.a
