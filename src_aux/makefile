####################################################################
#  Auxiliary routines makefile                                     #
#  Makefile for creating/updating auxiliary library object files   # 
#  for SLICOT on Windows platforms.                                #
#                                                                  #
#  SLICOT, Release 5.8                  .\slicot\src_aux\makefile  #
#  Vasile Sima                                                     #
#  Jan. 24, 2005.                                                  #
#  Revised Sep. 26, 2009; Jan. 25, 2010, Jan. 2016, Feb. 2023      #
####################################################################
#
#  This is the makefile to create/update the auxiliary library lpkaux.lib.
#  This library contains deprecated LAPACK routines dlatzm and zlatzm.
#
#  The command
#       nmake
#  without any arguments creates or updates the library
#       lpkaux.lib
#  in the next higher directory level.
#
#  To remove the object files after the library is created, enter
#       nmake clean
#
#######################################################################
 
!include <..\make.inc>

.SUFFIXES: .f .obj

DSLSRC = \
    dlatzm.obj zlatzm.obj

all: double

double: $(DSLSRC)
	$(ARCH) $(ARCHFLAGS):$(LPKAUXLIB) $(DSLSRC)

clean:
	del *.obj

.f.obj:
	$(FORTRAN) $(OPTS) -c $<
