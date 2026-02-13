C
C SPDX-License-Identifier: BSD-3-Clause
C
      SUBROUTINE NF01BE( IFLAG, NSMP, N, IPAR, LIPAR, Z, LDZ, Y, LDY, X,
     $                   NFEVL, E, J, LDJ, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     This is the FCN routine for optimizing the parameters of the
C     nonlinear part of a Wiener system (initialization phase), using
C     SLICOT Library routine MD03BD. See the argument FCN in the
C     routine MD03BD for the description of parameters. Note that
C     NF01BE is called for each output of the Wiener system.
C
C     Arguments
C
C      IFLAG   (input/output) INTEGER
C              On entry, this parameter must contain a value
C              defining the computations to be performed:
C              = 0 :  Optionally, print the current iterate X,
C                     function values E, and Jacobian matrix J,
C                     or other results defined in terms of these
C                     values. See the argument NPRINT of MD03BD.
C                     Do not alter E and J.
C              = 1 :  Calculate the functions at X and return
C                     this vector in E. Do not alter J.
C              = 2 :  Calculate the Jacobian at X and return
C                     this matrix in J. Also return NFEVL
C                     (see below). Do not alter E.
C              = 3 :  Do not compute neither the functions nor
C                     the Jacobian, but return in LDJ and
C                     IPAR/DPAR1,DPAR2 (some of) the integer/real
C                     parameters needed.
C              On exit, the value of this parameter should not be
C              changed by FCN unless the user wants to terminate
C              execution of MD03BD, in which case IFLAG must be
C              set to a negative integer.
C
C      NSMP    (input) INTEGER
C              The number of functions.  NSMP >= 0.
C
C      N       (input) INTEGER
C              The number of variables.  M >= N >= 0.
C
C      IPAR    (input/output) INTEGER array, dimension (LIPAR)
C              The integer parameters describing the structure of
C              the Jacobian matrix or needed for problem solving.
C              IPAR is an input parameter, except for IFLAG = 3
C              on entry, when it is also an output parameter.
C              On exit, if IFLAG = 3, IPAR(1) contains the length
C              of the array J, for storing the Jacobian matrix,
C              and the entries IPAR(2:5) contain the workspace
C              required by FCN for IFLAG = 1, FCN for IFLAG = 2,
C              QRFACT, and LMPARM, respectively.
C
C      LIPAR   (input) INTEGER
C              The length of the array IPAR.  LIPAR >= 5.
C
C      Z       (input/output) DOUBLE PRECISION array, dimension
C              (LDZ,*) or (LDZ)
C              A first set of real parameters needed for
C              describing or solving the problem.
C              DPAR1 can also be used as an additional array for
C              intermediate results when computing the functions
C              or the Jacobian. For control problems, DPAR1 could
C              store the input trajectory of a system.
C
C      LDZ     (input) INTEGER
C              The leading dimension or the length of the array
C              LDZ, as convenient.  LDZ  >= 0.  (LDZ >= 1,
C              if leading dimension.)
C
C      Y       (input/output) DOUBLE PRECISION array, dimension
C              (LDY,*) or (LDY)
C              A second set of real parameters needed for
C              describing or solving the problem.
C              DPAR2 can also be used as an additional array for
C              intermediate results when computing the functions
C              or the Jacobian. For control problems, DPAR2 could
C              store the output trajectory of a system.
C
C      LDY     (input) INTEGER
C              The leading dimension or the length of the array
C              LDY, as convenient.  LDY >= 0.  (LDY >= 1,
C              if leading dimension.)
C
C      X       (input) DOUBLE PRECISION array, dimension (N)
C              This array must contain the value of the
C              variables x where the functions or the Jacobian
C              must be evaluated.
C
C      NFEVL   (input/output) INTEGER
C              The number of function evaluations needed to
C              compute the Jacobian by a finite difference
C              approximation.
C              NFEVL is an input parameter if IFLAG = 0, or an
C              output parameter if IFLAG = 2. If the Jacobian is
C              computed analytically, NFEVL should be set to a
C              non-positive value.
C
C      E       (input/output) DOUBLE PRECISION array,
C              dimension (M)
C              This array contains the value of the (error)
C              functions e evaluated at X.
C              E is an input parameter if IFLAG = 0 or 2, or an
C              output parameter if IFLAG = 1.
C
C      J       (input/output) DOUBLE PRECISION array, dimension
C              (LDJ,NC), where NC is the number of columns
C              needed.
C              This array contains a possibly compressed
C              representation of the Jacobian matrix evaluated
C              at X. If full Jacobian is stored, then NC = N.
C              J is an input parameter if IFLAG = 0, or an output
C              parameter if IFLAG = 2.
C
C      LDJ     (input/output) INTEGER
C              The leading dimension of array J.  LDJ >= 1.
C              LDJ is essentially used inside the routines FCN,
C              QRFACT and LMPARM.
C              LDJ is an input parameter, except for IFLAG = 3
C              on entry, when it is an output parameter.
C              It is assumed in MD03BD that LDJ is not larger
C              than needed.
C
C      DWORK   (input/output) DOUBLE PRECISION array, dimension (LDWORK)
C              The workspace array for subroutine FCN.
C              On exit, if INFO = 0, DWORK(1) returns the optimal
C              value of LDWORK.
C
C      LDWORK  (input) INTEGER
C              The size of the array DWORK (as large as needed
C              in the subroutine FCN).  LDWORK >= 1.
C
C      INFO    (output) INTEGER
C              Error indicator, set to a negative value if an
C              input (scalar) argument is erroneous, and to
C              positive values for other possible errors in the
C              subroutine FCN. The LAPACK Library routine XERBLA
C              should be used in conjunction with negative INFO.
C              INFO must be zero if the subroutine finished
C              successfully.
C
C
C     ******************************************************************
C
C     .. Parameters ..
C     .. CJTE is initialized to avoid the calculation of J'*e ..
C     .. NOUT is the unit number for printing intermediate results ..
      CHARACTER         CJTE
      PARAMETER         ( CJTE = 'N' )
      INTEGER           NOUT
      PARAMETER         ( NOUT = 6 )
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IFLAG, INFO, LDJ, LDWORK, LDY, LDZ, LIPAR, N,
     $                  NFEVL, NSMP
C     .. Array Arguments ..
      INTEGER           IPAR(*)
      DOUBLE PRECISION  DWORK(*), E(*), J(LDJ,*), X(*), Y(LDY,*),
     $                  Z(LDZ,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  ERR
C     .. External Functions ..
      DOUBLE PRECISION  DNRM2
      EXTERNAL          DNRM2
C     .. External Subroutines ..
      EXTERNAL          DAXPY, NF01AY, NF01BY
C
C     .. Executable Statements ..
C
      INFO = 0
      IF ( IFLAG.EQ.1 ) THEN
C
C        Call NF01AY to compute the output y of the Wiener system (in E)
C        and then the error functions (also in E). The array Z must
C        contain the output of the linear part of the Wiener system, and
C        Y must contain the original output Y of the Wiener system.
C        IPAR(2) must contain the number of outputs.
C        Workspace: need:    2*NN, NN = IPAR(3) (number of neurons);
C                   prefer:  larger.
C
         CALL NF01AY( NSMP, IPAR(2), 1, IPAR(3), LIPAR-2, X, N, Z, LDZ,
     $                E, NSMP, DWORK, LDWORK, INFO )
         CALL DAXPY( NSMP, -ONE, Y, 1, E, 1 )
         DWORK(1) = 2*IPAR(3)
C
      ELSE IF ( IFLAG.EQ.2 ) THEN
C
C        Call NF01BY to compute the Jacobian in a compressed form.
C        IPAR(2), IPAR(3) must have the same content as for IFLAG = 1.
C        Workspace: need:    0.
C
         CALL NF01BY( CJTE, NSMP, IPAR(2), 1, IPAR(3), LIPAR-2, X, N, Z,
     $                LDZ, E, J, LDJ, DWORK, DWORK, LDWORK, INFO )
         NFEVL = 0
         DWORK(1) = ZERO
C
      ELSE IF ( IFLAG.EQ.3 ) THEN
C
C        Set the parameter LDJ, the length of the array J, and the sizes
C        of the workspace for FCN (IFLAG = 1 or 2), QRFACT and LMPARM.
C
         LDJ     = NSMP
         IPAR(1) = NSMP*N
         IPAR(2) = 2*IPAR(3)
         IPAR(3) = 0
         IPAR(4) = 4*N + 1
         IPAR(5) = 4*N
C
      ELSE IF ( IFLAG.EQ.0 ) THEN
C
C        Special call for printing intermediate results.
C
         ERR = DNRM2( NSMP, E, 1 )
         WRITE( NOUT, '('' Norm of current error = '', D15.6)') ERR
      END IF
      RETURN
C
C *** Last line of NF01BE ***
      END
