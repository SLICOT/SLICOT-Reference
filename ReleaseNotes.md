# SLICOT Library Release Notes

## Version v5.9 

Version v5.9 of the **SLICOT** Library includes 11 new routines and 9 updated routines, in comparison to Version v5.8 of the library.

**New Routines**

**`AB13HD:`**  Computes the L_infinity-norm of a standard or descriptor system using structure-preserving eigenvalue computations.

**`MA01DD:`**  Computes an approximate symmetric chordal metric, 
            
               d = min{ |a - b|, |1/a - 1/b| }
            
for two complex numbers `a` and `b`, given by real and imaginary parts.

**`MA01DZ:`**  Computes an approximate symmetric chordal metric `d` for two, possibly infinite, complex numbers `a` and `b`, given by fractions with non-negative denominators.

**`MA02RD:`**  Sorts the elements of a vector in increasing or decreasing order, and rearranges the elements of another vector using the same permutations.

**`MA02SD:`**  Computes the smallest nonzero absolute value of the elements of a real matrix.

**`MB04RD:`**  Reduces a real matrix pair `(A,B)` in generalized real Schur form to a block-diagonal form using well-conditioned non-orthogonal equivalence transformations. The condition numbers of the transformations used for reduction are roughly bounded by `PMAX`, where `PMAX >= 1` is a given real value. The transformations are optionally postmultiplied in two given matrices `X` and `Y`. The generalized Schur form is optionally ordered, so that clustered eigenvalues are grouped in the same block.

**`MB04RS:`**  Attempts to solve the generalized real Sylvester equation

               A * R - L * B = scale * C,                            (1)
               D * R - L * E = scale * F,

using Level 1 and Level 2 BLAS, but aborts the calculations when the absolute value of an element of `R` or `L` is greater than a given real number `PMAX >= 1`.

**`MB04RT:`**  Attempts to solve the generalized real Sylvester equation (1) using Level 3 BLAS, but aborts the calculations when the absolute value of an element of `R` or `L` is greater than a given real number `PMAX >= 1`.

**`MB04RV:`**  Attempts to solve the generalized complex Sylvester equation (1) using Level 1 and Level 2 BLAS, but aborts the calculations when the absolute value of an element of `R` or `L` is greater than a given real number `PMAX >= 1`.

**`MB04RW:`**  Attempts to solve the generalized complex Sylvester equation (1) using Level 3 BLAS, but aborts the calculations when the absolute value of an element of `R` or `L` is greater than a given real number `PMAX >= 1`.

**`MB04RZ:`**  Reduces a complex matrix pair `(A,B)` in generalized complex Schur form to a block-diagonal form using well-conditioned non-unitary equivalence transformations. The condition numbers of the transformations used for reduction are roughly bounded by `PMAX`, where `PMAX >= 1` is a given real value. The transformations are optionally postmultiplied in two given matrices `X` and `Y`. The generalized Schur form is optionally ordered, so that clustered eigenvalues are grouped in the same block.

**Updated Routines**

**`AB13DX:`**  Replaced `CWORK` and `LCWORK` by `ZWORK` and `LZWORK`, respectively, to agree to the **SLICOT** standards. Made several cosmetical changes.

**`AB13ID:`**  Placed the lines defining the external subroutines after the lines defining the external functions, to agree to the **SLICOT** standard. Replaced `TOLDEF` by `ZERO` in the `MB03OD` call with `LDWORK = -1`. Changed the tests for checking if the estimated rank might be incorrect. The test for setting the value 1 to `IWARN` has been modified for better significance and `IWARN` parameter description has been updated. The submatrix `R22` has also been set to zero. Used a safeguard against the huge workspace size returned by some implementations of the LAPACK routine `ZGESVD`. Made several cosmetical changes.

**`MA02JD:`**  Placed the lines defining the external subroutines after the lines defining the external functions, to agree to the **SLICOT** standard.
    
**`MA02JZ:`**  Placed the lines defining the external subroutines after the lines defining the external functions, to agree to the **SLICOT** standard.
    
**`MB02SZ:`**  Replaced the references to the `DCABS1` routine, which is not available in some `BLAS` implementations, to equivalent references to a locally defined statement functions.

**`MB4DLZ:`**  Replaced the references to the `DCABS1` routine, which is not available in some `BLAS` implementations, to equivalent references to a locally defined statement functions.

**`SB03OD:`**  Inserted a call to `DGERQF` for computing the optimal workspace when `TRANS = 'T'`.  Without this call, an error could appear when `M > N`.

**`SB03OZ:`**  Inserted a call to `ZGERQF` for computing the optimal workspace when `TRANS = 'C'`.  Without this call, an error could appear when `M > N`.

**`TG01JD:`**  Replaced the argument `ZERO` of the `TG01AD` call in the line 385 by a value computed based on the norms of the system matrices.

**Updated makefile**

The files **`makefile`** and **`makefile_Unix`** in the **`src`** sub-directory have been updated to include the new routines. 

**New and Updated Documentation Files**

Documentation files have been included for the new routines, and the existing documentation files have been updated when needed.

## Version v5.8, Update 1

**Updated Routines**

**`MB01PD:`** Removed `SAVE` statement and variable `FIRST` to get a thread safe version.

**`MB03VY:`**  Set `A( ILO, ILO, J )` to 1 if `IHI = ILO` in the loop labelled 20.
 
**`MB04BD`**:  Changed some part of finding the number of infinite eigenvalues (replaced `DE(1,2)` by `DE(1,3)` in the line 495, and replaced `NINF = MAX( I, J )` by `NINF = MAX( I, J )/2` in the line 513).

**`MB04BP`**:  Changed some part of finding the number of infinite eigenvalues (replaced `DE(1,2)` by `DE(1,3)` in the line 509, and replaced `NINF = MAX( I, J )` by `NINF = MAX( I, J )/2` in the line 527).  Called **`MB04BD`** internally, if on entry `INFO = 0` or if `INFO < 0` and `M` is sufficiently small (`M <= NX`, with parameter `NX` set to 250).  Updated the documentation file, **`MB04BP.html`**.

**Updated makefile**

The files **`makefile`** in the **`examples`** and **`src`** sub-directories have been updated, by alphabetically ordering the names, and adding the missing ones.

**New files**

Added **`make.inc`** and **`makefile`** in the **`slicot`** directory for Windows platform with Intel Fortran compilers.

Added **`make_Unix.inc`** in **`slicot`** directory and **`makefile_Unix`** in **`slicot`** directory and subdirectories **`examples`** and **`src`**, for Unix-like platfoms with gfortran compiler.

Added the file **`Installation.txt`** in the **`slicot`** directory.

## Version v5.8 

Version v5.8 of the **SLICOT** Library includes 18 new routines and 14 routines with more or few changes in the operational part of the source code, in comparison to Version v5.7 of the library.  Moreover, the (comment) lines referring to the version number have been removed in all routines, example programs, and documentation files.

**New Routines**

**`MA02AZ:`** (Conjugate) transposes all or part of a two-dimensional complex matrix.

**`MB01UY:`** Computes one of the matrix products `T := alpha*op(T)*A`, or `T := alpha*A*op(T)`, where `alpha` is a scalar, `A` is an `M-by-N` matrix, `T` is a triangular matrix, and `op(T)` is either `T` or `T'` (the transpose of `T`). A block-row/column algorithm is used, if possible. The result overwrites the array `T`.

**`MB01UZ:`** Computes one of the matrix products `T := alpha*op(T)*A`, or `T := alpha*A*op(T)`, where `alpha` is a scalar, `A` is an `M-by-N` complex matrix, `T` is a complex triangular matrix, and `op(T)` is `T`, or `T'` (the transpose of `T`), or `conj(T')` (the conjugate transpose of `T`). A block-row/column algorithm is used, if possible. The result overwrites the array `T`.

**`MB03RW:`** Solves the Sylvester equation `-A*X + X*B = C`, where `A` and `B` are complex `M-by-M` and `N-by-N` matrices, respectively, in Schur form. This routine is intended to be called only by **SLICOT** Library routine `MB03RZ`. For efficiency purposes, the computations are aborted when the absolute value of an element of `X` is greater than a given value `PMAX`.
 
**`MB03RZ:`** Reduces an upper triangular complex matrix `A` (Schur form) to a block-diagonal form using well-conditioned non-unitary similarity transformations. The condition numbers of the transformations used for reduction are roughly bounded by `PMAX`, where `PMAX` is a given value. The transformations are optionally postmultiplied in a given matrix `X`. The Schur form is optionally ordered, so that clustered eigenvalues are grouped in the same block.
 
**`MB03VW:`** Reduces the general product `A(:,:,1)^S(1) * A(:,:,2)^S(2) * ... * A(:,:,K)^S(K)` to upper Hessenberg-triangular form, where `A` is `N-by-N-by-K` and `S` is the signature array with values 1 or -1 (as exponents). The `H`-th matrix of `A` is reduced to upper Hessenberg form while the other matrices are triangularized.  Optionally, all or part of the transformation matrices are accumulated or updated.

**`SB03OS:`** Solves for `X = op(U)^H * op(U)` either the stable non-negative definite continuous-time Lyapunov equation

                    H                     2      H
               op(S) *X + X*op(S) = -scale *op(R) *op(R),

 or the convergent non-negative definite discrete-time Lyapunov equation

                    H                     2      H
               op(S) *X*op(S) - X = -scale *op(R) *op(R),

 where `op(K) = K` or `K^H` (the conjugate transpose of the matrix `K`), `S`, `R`, and `U` are `N-by-N` upper triangular matrices, and scale is an output scale factor, set less than or equal to 1 to avoid overflow in `X`.

**`SB03OZ:`** Solves for `X = op(U)^H * op(U)` either the stable non-negative definite continuous-time Lyapunov equation

                    H                     2      H
               op(A) *X + X*op(A) = -scale *op(B) *op(B),

 or the convergent non-negative definite discrete-time Lyapunov equation

                    H                     2      H
               op(A) *X*op(A) - X = -scale *op(B) *op(B),

 where `op(K) = K` or `K^H`  (the conjugate transpose of the matrix `K`), `A` is an `N-by-N` matrix, `op(B)` is an `M-by-N` matrix, `U` is an upper triangular matrix containing the Cholesky factor of the solution matrix `X`, and scale is an output scale factor, set less than or equal to 1 to avoid overflow in `X`.

**`SG03BR:`** Computes the parameters for the complex Givens rotation (`c` real, `s` complex) to annihilate the second element of a complex vector of length two. The first element of the rotated vector may be complex. This is a safer implementation of the previous **SLICOT** routine `SG03BY`. It is an adaptation for real double precision computations of the LAPACK routine `ZLARTG`.

**`SG03BS:`** Computes the Cholesky factor `U` of the matrix `X`, `X = op(U)^H * op(U)`, which is the solution of the generalized `d`-stable  discrete-time Lyapunov equation

                H            H                  2    H
               A  * X * A - E  * X * E = - SCALE  * B  * B,
 
 or the conjugate transposed equation 

                        H            H          2        H
               A * X * A  - E * X * E  = - SCALE  * B * B ,
 
 respectively, where `A`, `E`, `B`, and `U` are complex `N-by-N` matrices, and `SCALE` is an output scale factor, set less than or equal to 1 to avoid overflow in `X`. The Cholesky factor `U` of the solution is computed without first finding `X`. The pencil `A - lambda * E` must be in complex generalized Schur form (`A` and `E` are upper triangular and the diagonal elements of `E` are non-negative real numbers). Moreover, it must be `d`-stable, i.e., the moduli of its eigenvalues must be less than one. `B` must be an upper triangular matrix with real non-negative entries on its main diagonal. The resulting matrix `U` is upper triangular. The entries on its main diagonal are non-negative.
 
**`SG03BT:`** Computes the Cholesky factor `U` of the matrix `X`, `X = op(U)^H * op(U)`, which is the solution of the generalized `c`-stable continuous-time Lyapunov equation

                H            H                  2    H
               A  * X * E + E  * X * A = - SCALE  * B  * B,
 
 or the conjugate transposed equation

                        H            H          2        H
               A * X * E  + E * X * A  = - SCALE  * B * B ,
 
 respectively, where `A`, `E`, `B`, and `U` are complex `N-by-N` matrices, and `SCALE` is an output scale factor, set less than or equal to 1 to avoid overflow in `X`. The Cholesky factor `U` of the solution is computed without first finding `X`. The pencil `A - lambda * E` must be in complex generalized Schur form (`A` and `E` are upper triangular and the diagonal elements of `E` are non-negative real numbers). Moreover, it must be `c`-stable, i.e., its eigenvalues must have negative real parts. `B` must be an upper triangular matrix with real non-negative entries on its main diagonal. The resulting matrix `U` is upper triangular. The entries on its main diagonal are non-negative.
 
**`SG03BZ:`** Computes the Cholesky factor `U` of the matrix `X`, `op(U)^H * op(U)`, which is the solution of either the generalized `c`-stable continuous-time Lyapunov equation

                    H                    H                      2        H
               op(A)  * X * op(E) + op(E)  * X * op(A) = - SCALE  * op(B)  * op(B),
 
 or the generalized `d`-stable discrete-time Lyapunov equation
 
                    H                    H                      2        H
               op(A)  * X * op(A) - op(E)  * X * op(E) = - SCALE  * op(B)  * op(B),
 
 without first finding `X` and without the need to form the matrix `op(B)^H * op(B)`. `op(K)` is either `K` or `K^H` for `K = A`, `B`, `E`, `U`. `A` and `E` are `N-by-N` matrices, `op(B)` is an `M-by-N` matrix. The resulting matrix `U` is an `N-by-N` upper triangular matrix with non-negative entries on its main diagonal. `SCALE` is an output scale factor set to avoid overflow in `U`.

**`TG01KD, TG01KZ:`** Compute for a single-input single-output descriptor system, `(A, E, B, C)`, with `E` upper triangular, a transformed system, `(Q'*A*Z, Q'*E*Z, Q'*B, C*Z)`, via an orthogonal/unitary equivalence transformation, so that `Q'*B` has only the first element nonzero and `Q'*E*Z` remains upper triangular. `TG01KZ` is the complex version.

**`TG01OA, TG01OB:`** Compute for a single-input single-output descriptor system, `(A, E, B, C)`, with `E` upper triangular, a transformed system, `(Q'*A*Z, Q'*E*Z, Q'*B, C*Z)`, via an orthogonal/unitary equivalence transformation, so that `Q'*B` has only the first element nonzero and `Q'*E*Z` remains upper triangular. `TG01OA` is the real version and `TG01OB` is the complex version. `A`, `B`, `C` are stored in an array as the block elements `(2,2)`, `(2,1)`, and `(1,2)`, respectively, and `Q` and `Z` are not accumulated. These are the main differences with **SLICOT** Library routines `TG01KD` and `TG01KZ`.

**`TG01OD, TG01OZ:`** Compute for a single-input single-output descriptor system, `(A, E, B, C)`, with `E` nonsingular, a reduced system with a "sufficiently" large feedthrough variable, using `TG01OA/TG01OB`.

**Updated Routines**

**`AB13MD:`** Computed the correct upper bound on the structured singular value for a 1-by-1 real matrix and a real uncertainty. Replaced `DFLOAT` by `DBLE` in line `834` to avoid a trouble with an Apple Silicon compiler.

**`MA02EZ:`** A new option, `SKEW = 'G'`, has been added that allows to suitably deal with the diagonal of a general square triangular matrix. This option is needed in the new routines `MB01UZ`, `SG03BS`, and `SG03BT`. Moreover, the internal loop index `J` has been modified from `2` to `I` or `I+1`, to reduce the number of cycles to the minimum values.

**`MB03RD:`** Replaced `PMAX*PMAX` by `PMAX` in the comments;  the `PMAX` value agrees to the condition number for the optimally scaled transformations used.

**`MB04BD:`** Made a correction (in comments) of the indices of the `1-by-1` or `2-by-2` quadruple diagonal blocks stored in `DWORK`. Increased `I2X2` by `1` if a `2x2` quadruple of diagonal blocks is found to have real eigenvalues (in order to check them externally). These are stored as `2x2` quadruple blocks in DWORK in that case. Made two corrections of the pointers to the locations in `DWORK` storing the quadruples with unreliable eigenvalues.
 
**`SB03OD:`** Many changes have been made to improve the routine, the main ones being summarized below:
  - Added code segments to control overflow. In essence, two scaling strategies are included. One strategy scales `A` and `B` if the maximum absolute value of their elements are outside a range `[SMLNUM,BIGNUM]`, where `SMLNUM = sqrt( SAFMIN )/EPS`, `BIGNUM = 1/SMLNUM`, `SAFMIN` is the safe minimum, and `EPS` is the machine accuracy. The second strategy, invoked for continuous-time equations, scales `A` and `B` if the maximum absolute values of `A` and `B` differ too much, or their minimum (maximum) is too large (small, respectively);  specifically,  this scaling is performed if `MN < MX*SMLNUM`, or `MX < SMLNUM`, or `MN > BIGNUM`, where `MN` and `MX` are the minimum and maximum, respectively, of the maximum absolute values of `A` and `B`.  Both strategies are effective and ensure the same accuracy of the results, but the second strategy reduces the number of instances when the output scaling factor, `SCALE`, is strictly smaller than `1`. Scaling of `B` is done before computing its initial `QR` or `RQ` factorization if the maximum absolute value of its elements is greater than `1/SAFMIN`; otherwise, it is done after `QR/RQ` factorization. The implementation checks out first the conditions for the second scaling strategy.
  - The auxiliary routine `SB03OU` is no longer called, and all its computations, and additional ones, are performed by `SB03OD`.  Two `QR` or `RQ` factorizations for `B` and `R*Q` or `Q'*R`, respectively (where `R` is the triangular factor of the first factorization) are done only if `M > 7*N/6`;  otherwise, a single `QR` or `RQ` factorization is used.
  - The new routine `MB01UY` is called by `SB03OD` to compute the product `R*Q` or `Q*R`, overwriting the array containing `R`;  as large block-row or block-column operations as possible (depending on the workspace length) are used.  
  - More BLAS 3 `DGEMM` operations are used when updating the given `B` as `B*Q` or `Q'*B`, using again as large block-row or block-column as possible.  The previous version used BLAS 2 `DGEMV` calls if the workspace length was smaller than `N*M`.
  - The eigenvalues of `A` are computed even if `A` is given in the real Schur form, by calling the LAPACK routine `DLANV2`.  This way, the stability of `A` can be checked out also for the option `FACT = 'F'`.
  - The case when `Q` is an identity matrix is detected, to avoid its use in multiplications.
  - The postponed scaling of transformed `B` is dealt with separately for the cases `TRANS = 'N'` and `TRANS = 'T'`.
  - The minimal workspace size has been reduced by `min(N,M)`.
 The improvements allowed to reduce the computation times comparing to the previous version of this routine, sometimes by `10%`, or even `20%`.  Many changes in the comments have also been performed.

**`SB03OT:`** Deleted the lines 509-512 involving a test of the variable `TEMP` (compared to `SMIN`), which proved to be unnecessary.

**`SB03OY:`** Replaced the definition of `SMIN` in the lines 211-212 by `SMLNUM`.  The former definition set `SMIN` to a too large value for equations with elements of big magnitude, and the results were bad.

**`SG03BD:`** Many changes have been made to improve the routine, the main ones being summarized below:
  - Added code segments to control overflow. In essence, two scaling strategies are included.  One strategy scales `A`, `E`, and `B` if the maximum absolute value of their elements are outside a range `[SMLNUM,BIGNUM]`, where `SMLNUM = sqrt( SAFMIN )/EPS`, `BIGNUM = 1/SMLNUM`, `SAFMIN` is the safe minimum, and `EPS` is the machine accuracy. The second strategy scales `A`, `E`, and `B` if the maximum absolute values of `A`, `E`, and `B` differ too much, or if their minimum (maximum) is too large (small, respectively); specifically, this scaling is performed if `MN < MX*SMLNUM`, or `MX < SMLNUM`, or `MN > BIGNUM`, where `MN` and `MX` are the minimum and maximum, respectively, of the maximum absolute values of `A`, `E`, and `B`.  Both strategies are effective and ensure the same accuracy of the results, but the second strategy reduces the number of instances when the output scaling factor, `SCALE`, is strictly smaller than `1`. Scaling of `B` is done before computing its initial `QR` or `RQ` factorization if the maximum absolute value of its elements is greater than `1/SAFMIN`; otherwise, it is done after `QR/RQ` factorization.  The implementation checks out first the conditions for the second scaling strategy.  The scaling factors of `E` may be set equal to those for `A`, to preserve stability in the discrete-time case. 
  - Modified the sequences for the transformation of the right hand side, to use BLAS 3 `DGEMM` calls (possibly in a loop, if workspace is not large enough).
  - Used the new routine, `MB01UY`, for transforming the solution back by block-row or block-column operations. The result overwrites the array storing the upper triangular part of the reduced equation solution.
 - The call of `DGGES` in the sequence to find the optimal workspace is omitted if `A` and `E` are given in generalized Schur form.
 - Avoided the back transformation operations with the matrices `Q` and `Z`, if they are identity.
 - Made some additional comments on the output contents of `A`, `E`, `Q`, and `Z` arrays, as well as on the input contents of `Q` and `Z` arrays when `FACT = 'F'`.

**`SG03BU:`** Safer computation of the 1-by-1 diagonal blocks of the solution. Replaced loops of `DSCAL` calls by calls to the LAPACK routine `DLASCL`. Replaced the calls to BLAS 1 routine `DROTG` by calls to the safer LAPACK routine `DLARTG`.

**`SG03BV:`** Safer computation of the 1-by-1 diagonal blocks of the solution, to avoid overflows for badly scaled equations. Replaced loops of `DSCAL` calls by calls to the LAPACK routine `DLASCL`. Replaced the calls to BLAS 1 routine `DROTG` by calls to the safer LAPACK routine `DLARTG`.

**`SG03BW:`** Replaced the calls to the **SLICOT** routines `MB02UV` and `MB02UU` by calls to the equivalent LAPACK routines `DGETC2` and `DGESC2`, respectively. Replaced loops of `DSCAL` calls by calls to the LAPACK routine `DLASCL`. Replaced some loops of assignment statements by calls to `DCOPY`. 
 
**`SG03BX:`** The code has practically been rewritten. Although the source code of the new version is somewhat longer than of the previous version, the object code was reduced to about a half of the initial one.
  - Replaced all `2-by-2` local arrays by the needed scalars, and replaced the calls to the BLAS routines `DGEMM` and `DGEMV` involving these arrays by the necessary scalar operations, made in a compact manner. In particular, all rotations are applied directly, updating only the needed elements. When possible, two rotations are applied at once (from the left and right). Also, only the necessary elements of the matrices M1 and M2 of the reduced equations are now computed.
  - Replaced the calls to the **SLICOT** routine `SG03BY` by calls to the new, safer routine `SG03BR`.
  - Added a sequence of code to standardize the matrix `E`, i.e., to become diagonal with `E(1,1)` non-negative; then recomputed the shift.
  - Safer computation of the `1-by-1` diagonal blocks of the solution, to avoid overflows for badly scaled equations.
  - Used simpler formulas and computations when numerically appropriate, but more sophisticated ones are invoked when needed.
  - When `E(1,1)` is not very small for discrete-time equation, an alternative approach is employed for computing the trailing diagonal element of the solution; this approach factors a rank-one Hermitian matrix by solving a special symmetric eigenvalue problem. The eigenvector corresponding to eigenvalue `1` is quickly found.
 
**`TB01MD, TB01ND:`** Moved the sequence for initializing `U` before the Quick Return section, to set `U` to identity also for the case `M = 0` or `P = 0`, respectively.

**`TB01ND:`** Added the condition `P <= N`, since the observer Hessenberg form is not defined for `P > N`.

## Version v5.7 

This is the first version uploaded to GitHub under a BSD-3-Clause license. 

