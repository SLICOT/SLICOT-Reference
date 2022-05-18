# SLICOT Library

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6463090.svg)](https://doi.org/10.5281/zenodo.6463090)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](https://github.com/SLICOT/SLICOT-Reference/blob/main/LICENSE)

**SLICOT** - _Subroutine Library In COntrol Theory_ - is a general purpose basic
mathematical library for control theoretical computations.  The library
provides tools to perform essential system analysis and synthesis tasks.
The main emphasis in SLICOT is on numerical reliability of implemented
algorithms and the numerical robustness and efficiency of routines.
Providing algorithmic flexibility and the use of rigorous implementation
and documentation standards are other SLICOT features.

The SLICOT Library is available as standard Fortran 77 code in double
precision.  Each user-callable subroutine for control computations is
accompanied by an example program which illustrates the use of the
subroutine and can act as a template for the user's own routines.

The SLICOT Library is organized by chapters, sections and subsections.
The following chapters are currently included:

A : Analysis Routines

B : Benchmark and Test Problems

D : Data Analysis

F : Filtering

I : Identification

M : Mathematical Routines

N : Nonlinear Systems
    (not yet available, except for some auxiliary routines for Wiener systems)
    
S : Synthesis Routines

T : Transformation Routines

U : Utility Routines

SLICOT Library Root Directory contains few, basic files for the SLICOT Library
distribution and generation.  When distributed, SLICOT software comes with
several filled-in subdirectories (benchmark_data, doc, examples, src, and src_aux),
and the following files in this root directory:

- this file, README.md,

- the contributors to the library and financial support, Contributors.md,

- the license file, LICENSE, and

- the main SLICOT Library documentation index, libindex.html.

After software installation, this directory will also contain the library 
file slicot.a or slicot.lib, for Unix or Windows platforms, respectively.
The library file could then be linked in applications programs, as usual.
Specific examples are contained in the directory examples.
The on-line documentation of the SLICOT user's callable routines is
accessible via the main SLICOT Library documentation index, libindex.html.
This file also contains a link to the documentation of the lower-level,
support routines.

The SLICOT Library is built on LAPACK (Linear Algebra PACKage) and BLAS
(Basic Linear Algebra Subprograms) collections.  Therefore, these
packages should be available on the platform used.

Basic References:

1. P. Benner, V. Mehrmann, V. Sima, S. Van Huffel, and A. Varga,
   "SLICOT - A Subroutine Library in Systems and Control Theory",
   Applied and Computational Control, Signals, and Circuits
   (Birkhauser), Vol. 1, Ch. 10, pp. 505-546, 1999.

2. S. Van Huffel, V. Sima, A. Varga, S. Hammarling, and F. Delebecque,
   "Development of High Performance Numerical Software for Control",
   IEEE Control Systems Magazine, Vol. 24, Nr. 1, Feb., pp. 60-76, 2004.
