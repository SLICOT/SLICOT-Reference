C
C SPDX-License-Identifier: BSD-3-Clause
C
      SUBROUTINE MD03BF( IFLAG, M, N, IPAR, LIPAR, DPAR1, LDPAR1, DPAR2,
     $                   LDPAR2, X, NFEVL, E, J, LDJ, DWORK, LDWORK,
     $                   INFO )
C
C     PURPOSE
C
C     This is the FCN routine for solving a standard nonlinear least
C     squares problem using SLICOT Library routine MD03BD. See the
C     parameter FCN in the routine MD03BD for the description of
C     parameters.
C
C     Arguments
C
C     IFLAG   (input/output) INTEGER
C             On entry, this parameter must contain a value
C             defining the computations to be performed:
C             = 0 :  Optionally, print the current iterate X,
C                    function values E, and Jacobian matrix J,
C                    or other results defined in terms of these
C                    values. See the argument NPRINT of MD03BD.
C                    Do not alter E and J.
C             = 1 :  Calculate the functions at X and return
C                    this vector in E. Do not alter J.
C             = 2 :  Calculate the Jacobian at X and return
C                    this matrix in J. Also return NFEVL
C                    (see below). Do not alter E.
C             = 3 :  Do not compute neither the functions nor
C                    the Jacobian, but return in LDJ and
C                    IPAR/DPAR1,DPAR2 (some of) the integer/real
C                    parameters needed.
C             On exit, the value of this parameter should not be
C             changed by FCN unless the user wants to terminate
C             execution of MD03BD, in which case IFLAG must be
C             set to a negative integer.
C
C     M       (input) INTEGER
C             The number of functions.  M >= 0.
C
C     N       (input) INTEGER
C             The number of variables.  M >= N >= 0.
C
C     IPAR    (input/output) INTEGER array, dimension (LIPAR)
C             The integer parameters describing the structure of
C             the Jacobian matrix or needed for problem solving.
C             IPAR is an input parameter, except for IFLAG = 3
C             on entry, when it is also an output parameter.
C             On exit, if IFLAG = 3, IPAR(1) contains the length
C             of the array J, for storing the Jacobian matrix,
C             and the entries IPAR(2:5) contain the workspace
C             required by FCN for IFLAG = 1, FCN for IFLAG = 2,
C             QRFACT, and LMPARM, respectively.
C
C     LIPAR   (input) INTEGER
C             The length of the array IPAR.  LIPAR >= 5.
C
C     DPAR1   (input/output) DOUBLE PRECISION array, dimension
C             (LDPAR1,*) or (LDPAR1)
C             A first set of real parameters needed for
C             describing or solving the problem.
C             DPAR1 can also be used as an additional array for
C             intermediate results when computing the functions
C             or the Jacobian. For control problems, DPAR1 could
C             store the input trajectory of a system.
C
C     LDPAR1  (input) INTEGER
C             The leading dimension or the length of the array
C             DPAR1, as convenient.  LDPAR1 >= 0.  (LDPAR1 >= 1,
C             if leading dimension.)
C
C     DPAR2   (input/output) DOUBLE PRECISION array, dimension
C             (LDPAR2,*) or (LDPAR2)
C             A second set of real parameters needed for
C             describing or solving the problem.
C             DPAR2 can also be used as an additional array for
C             intermediate results when computing the functions
C             or the Jacobian. For control problems, DPAR2 could
C             store the output trajectory of a system.
C
C     LDPAR2  (input) INTEGER
C             The leading dimension or the length of the array
C             DPAR2, as convenient.  LDPAR2 >= 0.  (LDPAR2 >= 1,
C             if leading dimension.)
C
C     X       (input) DOUBLE PRECISION array, dimension (N)
C             This array must contain the value of the
C             variables x where the functions or the Jacobian
C             must be evaluated.
C
C     NFEVL   (input/output) INTEGER
C             The number of function evaluations needed to
C             compute the Jacobian by a finite difference
C             approximation.
C             NFEVL is an input parameter if IFLAG = 0, or an
C             output parameter if IFLAG = 2. If the Jacobian is
C             computed analytically, NFEVL should be set to a
C             non-positive value.
C
C     E       (input/output) DOUBLE PRECISION array,
C             dimension (M)
C             This array contains the value of the (error)
C             functions e evaluated at X.
C             E is an input parameter if IFLAG = 0 or 2, or an
C             output parameter if IFLAG = 1.
C
C     J       (input/output) DOUBLE PRECISION array, dimension
C             (LDJ,NC), where NC is the number of columns
C             needed.
C             This array contains a possibly compressed
C             representation of the Jacobian matrix evaluated
C             at X. If full Jacobian is stored, then NC = N.
C             J is an input parameter if IFLAG = 0, or an output
C             parameter if IFLAG = 2.
C
C     LDJ     (input/output) INTEGER
C             The leading dimension of array J.  LDJ >= 1.
C             LDJ is essentially used inside the routines FCN,
C             QRFACT and LMPARM.
C             LDJ is an input parameter, except for IFLAG = 3
C             on entry, when it is an output parameter.
C             It is assumed in MD03BD that LDJ is not larger
C             than needed.
C
C     DWORK   (input/output) DOUBLE PRECISION array, dimension (LDWORK)
C             The workspace array for subroutine FCN.
C             On exit, if INFO = 0, DWORK(1) returns the optimal
C             value of LDWORK.
C
C     LDWORK  (input) INTEGER
C             The size of the array DWORK (as large as needed
C             in the subroutine FCN).  LDWORK >= 1.
C
C     INFO    (output) INTEGER
C             Error indicator, set to a negative value if an
C             input (scalar) argument is erroneous, and to
C             positive values for other possible errors in the
C             subroutine FCN. The LAPACK Library routine XERBLA
C             should be used in conjunction with negative INFO.
C             INFO must be zero if the subroutine finished
C             successfully.
C
C
C     The example programmed in this routine is adapted from that
C     accompanying the MINPACK routine LMDER.
C
C     ******************************************************************
C
C     .. Parameters ..
C     .. NOUT is the unit number for printing intermediate results ..
      INTEGER           NOUT
      PARAMETER         ( NOUT = 6 )
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IFLAG, INFO, LDJ, LDPAR1, LDPAR2, LDWORK, LIPAR,
     $                  M, N, NFEVL
C     .. Array Arguments ..
      INTEGER           IPAR(*)
      DOUBLE PRECISION  DPAR1(*), DPAR2(*), DWORK(*), E(*), J(LDJ,*),
     $                  X(*)
C     .. Local Scalars ..
      INTEGER           I
      DOUBLE PRECISION  ERR, TMP1, TMP2, TMP3, TMP4
C     .. External Functions ..
      DOUBLE PRECISION  DNRM2
      EXTERNAL          DNRM2
C     .. DATA Statements ..
      DOUBLE PRECISION  Y(15)
      DATA              Y(1), Y(2), Y(3), Y(4), Y(5), Y(6), Y(7), Y(8),
     $                  Y(9), Y(10), Y(11), Y(12), Y(13), Y(14), Y(15)
     $                  / 1.4D-1, 1.8D-1, 2.2D-1, 2.5D-1, 2.9D-1,
     $                    3.2D-1, 3.5D-1, 3.9D-1, 3.7D-1, 5.8D-1,
     $                    7.3D-1, 9.6D-1, 1.34D0, 2.1D0,  4.39D0 /
C
C     .. Executable Statements ..
C
      INFO = 0
      IF ( IFLAG.EQ.1 ) THEN
C
C        Compute the error function values.
C
         DO 10 I = 1, 15
            TMP1 = I
            TMP2 = 16 - I
            IF ( I.GT.8 ) THEN
               TMP3 = TMP2
            ELSE
               TMP3 = TMP1
            END IF
            E(I) = Y(I) - ( X(1) + TMP1/( X(2)*TMP2 + X(3)*TMP3 ) )
   10    CONTINUE
C
      ELSE IF ( IFLAG.EQ.2 ) THEN
C
C        Compute the Jacobian.
C
         DO 30 I = 1, 15
            TMP1 = I
            TMP2 = 16 - I
            IF ( I.GT.8 ) THEN
               TMP3 = TMP2
            ELSE
               TMP3 = TMP1
            END IF
            TMP4 = ( X(2)*TMP2 + X(3)*TMP3 )**2
            J(I,1) = -ONE
            J(I,2) = TMP1*TMP2/TMP4
            J(I,3) = TMP1*TMP3/TMP4
   30    CONTINUE
C
         NFEVL = 0
C
      ELSE IF ( IFLAG.EQ.3 ) THEN
C
C        Set the parameter LDJ, the length of the array J, and the sizes
C        of the workspace for FCN (IFLAG = 1 or 2), MD03BA and MD03BB.
C
         LDJ = M
         IPAR(1) = M*N
         IPAR(2) = 0
         IPAR(3) = 0
         IPAR(4) = 4*N + 1
         IPAR(5) = 4*N
C
      ELSE IF ( IFLAG.EQ.0 ) THEN
C
C        Special call for printing intermediate results.
C
         ERR = DNRM2( M, E, 1 )
         WRITE( NOUT, '('' Norm of current error = '', D15.6)') ERR
C
      END IF
C
      RETURN
C
C *** Last line of MD03BF ***
      END
